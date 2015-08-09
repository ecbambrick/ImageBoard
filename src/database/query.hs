{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Query where

import Control.Monad.State ( State, execState, state )
import Data.List           ( intercalate )

------------------------------------------------------------------------- Types

type FieldMapper = String -> Field
type Query = State (Int, QueryData)
type Value = String
type Name = String
type Alias = Int
type QueryResult a = (a, (Int, QueryData))

data Join    = BaseTable | InnerJoin Filter deriving (Show)
data Field   = Field Name Name deriving (Show)
data Table   = Table Name Alias Join deriving (Show)
data Mapping = Mapping Field Value deriving (Show)
data Order   = Asc Field | Desc Field | Random deriving (Show)

data Filter = Not Filter
            | Equals Field Value
            | Exists Select
            | Like Field String
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
    toSQL (Table name i (InnerJoin filter)) = "INNER JOIN " ++ name ++ " f" ++ show i ++ " ON " ++ toSQL filter
    
instance ToSQL Filter where
    toSQL (Not filter)         = "NOT " ++ toSQL filter
    toSQL (Equals field value) = toSQL field ++ " = " ++ value
    toSQL (Like field value)   = toSQL field ++ " LIKE " ++ value
    toSQL (Exists select)      = "EXISTS (" ++ toSQL select ++ ")"
    
instance ToSQL Select where
    toSQL (Select _ [] _ _) = ""
    toSQL Select {..} = intercalate "\n" $ filter (not . null)
        [ "SELECT " ++ selectClause
        , fromClause
        , whereClause
        , orderClause ]
        where 
        
            selectClause
                | null selectValues && length selectTables == 1 = "*"
                | null selectValues = intercalate ", " $ map selectAll selectTables
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

from :: String -> Query FieldMapper
from = state . addTable BaseTable

innerJoin :: String -> (FieldMapper -> Filter) -> Query FieldMapper
innerJoin name mapper = state $ \(i, q) -> 
    ( Field ("f" ++ show i)
    , (i+1, q { queryTables = queryTables q ++ [Table name i (InnerJoin (mapper (Field ("f" ++ show i))))] } ))
    
get :: [Field] -> Query ()
get = state . addValues

wherever :: Filter -> Query ()
wherever = state . addFilter

ignoring :: Filter -> Query ()
ignoring = wherever . Not

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

(.=) :: (ToSQL a) => Field -> a -> Filter
(.=) field val = Equals field (toSQL val)

like :: Field -> String -> Filter
like field val = Like field (escape val)
    where escape = (++ " ESCAPE '\\'") . toSQL

exists :: Query a -> Filter
exists = Exists . select

------------------------------------------------------ Transformation Functions

select :: Query a -> Select
select q = Select (queryValues  q') 
                  (queryTables  q')
                  (queryFilters q')
                  (queryOrders  q')
    where (i, q') = execState q emptyQuery

----------------------------------------------------------------------- Utility

emptyQuery = (0, QueryData [] [] [] [] [])

addTable :: Join -> String -> (Int, QueryData) -> QueryResult FieldMapper
addTable join name (i, q) = 
    ( Field ("f" ++ show i)
    , (i+1, q { queryTables = queryTables q ++ [Table name i join] } ))

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
