{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Query where

import Control.Monad.State ( State, execState, state, gets )
import Data.List           ( intercalate )

------------------------------------------------------------------------- Types

type FieldMapper = String -> Field
type Query = State (Int, QueryData)
type Value = String
type Name = String
type Alias = Int
type QueryResult a = (a, (Int, QueryData))

data Join    = BaseTable | InnerJoin (Maybe Filter) deriving (Show)
data Field   = Field Name Name deriving (Show)
data Table   = Table Name Alias Join deriving (Show)
data Mapping = Mapping Field Value deriving (Show)
data Order   = Asc Field | Desc Field | Random deriving (Show)

data Filter = Not Filter
            | Equals Field Value
            | Exists Select
            | Like Field String
            | And Filter Filter
            deriving (Show)

data QueryData = QueryData
    { queryValues   :: [Field]
    , queryMappings :: [Mapping]
    , queryTables   :: [Table]
    , queryFilters  :: [Filter]
    , queryOrders   :: [Order] } deriving (Show)

data Select = Select
    { selectValues   :: [Field]
    , selectTables   :: [Table]
    , selectFilters  :: [Filter]
    , selectOrders   :: [Order] } deriving (Show)

------------------------------------------------------------------------- ToSQL

replace :: Char -> String -> String -> String
replace _ _ [] = []
replace from to (x:xs)
    | x == from = to  ++ replace from to xs
    | otherwise = [x] ++ replace from to xs

class ToSQL a where
    toSQL :: a -> Value 

instance ToSQL Int where
    toSQL = show

instance ToSQL [Char] where
    toSQL x = "'" ++ replace '\'' "''" x ++ "'"

instance ToSQL Field where
    toSQL (Field table field) = table ++ "." ++ field

instance ToSQL Order where
    toSQL (Asc field)  = toSQL field ++ " ASC"
    toSQL (Desc field) = toSQL field ++ " DESC"
    toSQL (Random)     = "RANDOM()"

instance ToSQL Table where
    toSQL (Table name i BaseTable)          = "FROM " ++ name ++ " f" ++ show i
    toSQL (Table name i (InnerJoin filter)) = "INNER JOIN " ++ name ++ " f" ++ show i ++ case filter of
        Nothing     -> ""
        Just filter -> " ON " ++ toSQL filter
    
instance ToSQL Filter where
    toSQL (Not filter)         = "NOT " ++ toSQL filter
    toSQL (Equals field value) = toSQL field ++ " = " ++ value
    toSQL (Like field value)   = toSQL field ++ " LIKE " ++ value
    toSQL (Exists select)      = "EXISTS (" ++ toSQL select ++ ")"
    toSQL (And x y)            = toSQL x ++ " AND " ++ toSQL y
    
instance ToSQL Select where
    toSQL Select {..} = intercalate "\n" $ filter (not . null)
        [ "SELECT " ++ selectClause
        , fromClause
        , whereClause
        , orderClause ]
        where 
        
            selectClause
                | null selectTables = "1"
                | null selectValues = "*"
                | otherwise         = intercalate ", " $ map toSQL selectValues
                where selectAll (Table name i _) = "f" ++ show i ++ ".*"
        
            fromClause = intercalate "\n" $ map toSQL selectTables
            
            whereClause = if null selectFilters
                then ""
                else "WHERE " ++ (intercalate "\nAND " $ map toSQL selectFilters)
            
            orderClause = if null selectOrders
                then ""
                else "ORDER BY " ++ intercalate ", " (map toSQL selectOrders)

--------------------------------------------------------------- Query Functions

table :: String -> Query FieldMapper
table = state . addTable

on :: Query FieldMapper -> (FieldMapper -> Query Filter) -> Query FieldMapper
on mapper filter = do
    mapper' <- mapper
    filter' <- fmap Just (filter mapper')
    state $ \(i,q) ->
        let prevTables = init (queryTables q)
            lastTable  = last (queryTables q)
            name       = tableName lastTable
            nextTable  = [Table name (i-1) (InnerJoin filter')]
            
        in ( Field ("f" ++ show (i-1))
           , (i, q { queryTables = prevTables ++ nextTable } ))
    where tableName (Table name _ _) = name
    
get' :: [Field] -> Query ()
get' = state . addValues

wherever :: Query Filter -> Query ()
wherever = (>>= state . addFilter)

asc :: Field -> Query ()
asc = state . addOrder . Asc

desc :: Field -> Query ()
desc = state . addOrder . Desc

randomOrder :: Query ()
randomOrder = state (setOrder Random)

(.<-) :: (ToSQL a) => Field -> a -> Query ()
(.<-) field val = state $ addMapping $ Mapping field (toSQL val)

-------------------------------------------------------------- Helper Functions

with :: (ToSQL a) => String -> a -> FieldMapper -> Query ()
with name val field = state $ addFilter (Equals (field name) (toSQL val))

-------------------------------------------------------------- Filter Functions

(.=) :: (ToSQL a) => Field -> a -> Query Filter
(.=) field val = return $ Equals field (toSQL val)

infixr 2 .&
(.&) :: Query Filter -> Query Filter -> Query Filter
(.&) f1 f2 = do
    f1' <- f1
    f2' <- f2
    return (And f1' f2') 

like :: Field -> String -> Query Filter
like field val = return $ Like field (escape val)
    where escape = (++ " ESCAPE '\\'") . toSQL

exists :: Query a -> Query Filter
exists q = do
    i  <- gets fst
    
    let (i', q') = execState q (i, emptyQuery)
    
    let thing = Exists $ Select (queryValues  q') 
                                (queryTables  q')
                                (queryFilters q')
                                (queryOrders  q')
    
    state $ \(i,q) -> (thing, (i', q))

------------------------------------------------------ Transformation Functions

select :: Query a -> Select
select q = Select (queryValues  q') 
                  (queryTables  q')
                  (queryFilters q')
                  (queryOrders  q')
    where (i, q') = execState q (0, emptyQuery)

----------------------------------------------------------------------- Utility

emptyQuery = QueryData [] [] [] [] []

addTable :: String -> (Int, QueryData) -> QueryResult FieldMapper
addTable name (i, q) = 
    let existingTables = queryTables q
        newTable       = Table name i $ if null existingTables
                             then BaseTable
                             else InnerJoin Nothing
    in ( Field ("f" ++ show i)
       , (i+1, q { queryTables = existingTables ++ [newTable] } ))

addMapping :: Mapping -> (Int, QueryData) -> QueryResult ()
addMapping mapping (i, q) = ((), (i, q { queryMappings = queryMappings q ++ [mapping] }))

addOrder :: Order -> (Int, QueryData) -> QueryResult ()
addOrder order (i, q) = ((), (i, q { queryOrders = queryOrders q ++ [order] }))

addValues :: [Field] -> (Int, QueryData) -> QueryResult ()
addValues values (i, q) = ((), (i, q { queryValues = queryValues q ++ values }))

addFilter :: Filter -> (Int, QueryData) -> QueryResult ()
addFilter filter (i, q) = ((), (i, q { queryFilters = queryFilters q ++ [filter] }))

setOrder :: Order -> (Int, QueryData) -> QueryResult ()
setOrder order (i, q) = ((), (i, q { queryOrders = [order] }))
