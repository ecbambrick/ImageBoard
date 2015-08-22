{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Query where

import Control.Monad.State ( State, execState, state, gets )

------------------------------------------------------------------------- Types

type FieldMapper = String -> Field
type Query = State (Int, QueryData)
type Name = String
type Alias = Int
type QueryResult a = (a, (Int, QueryData))

data Join     = BaseTable | InnerJoin (Maybe Filter) deriving (Show)
data Field    = Field Alias Name deriving (Show)
data Table    = Table Name Alias Join deriving (Show)
data Mapping  = Mapping Name Value deriving (Show)
data Order    = Asc Field | Desc Field | Random deriving (Show)
data Likeness = Likeness String

data Value = SQLInt  Int
           | SQLStr  String
           | SQLBool Bool
           | SQLObj  Field
           deriving (Show)

data Filter = Not Filter
            | Equals Field Value
            | Exists Select
            | Like Field String
            | And Filter Filter
            deriving (Show)

data QueryData = QueryData
    { queryValues   :: [Field]
    , queryTables   :: [Table]
    , queryFilters  :: [Filter]
    , queryOrders   :: [Order] } deriving (Show)

data Select = Select
    { selectValues   :: [Field]
    , selectTables   :: [Table]
    , selectFilters  :: [Filter]
    , selectOrders   :: [Order] } deriving (Show)

----------------------------------------------------------------------- ToValue

class ToValue a where 
    toValue :: a -> Value

instance ToValue Int    where toValue = SQLInt
instance ToValue String where toValue = SQLStr
instance ToValue Field  where toValue = SQLObj 
instance ToValue Bool   where toValue = SQLBool

--------------------------------------------------------------- Query Functions

mappingField (Mapping field _) = field
mappingValue (Mapping _ value) = value

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
            
        in ( Field (i-1)
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

(.<-) :: (ToValue a) => String -> a -> Mapping
(.<-) field val = Mapping field (toValue val)

-------------------------------------------------------------- Filter Functions

(.=) :: (ToValue a) => Field -> a -> Query Filter
(.=) field val = return $ Equals field (toValue val)

infixr 2 .&
(.&) :: Query Filter -> Query Filter -> Query Filter
(.&) f1 f2 = do
    f1' <- f1
    f2' <- f2
    return (And f1' f2') 

like :: Field -> String -> Query Filter
like field val = return $ Like field val

exists :: Query a -> Query Filter
exists q = do
    i  <- gets fst
    
    let (i', q') = execState q (i, emptyQuery)
    
    let thing = Exists $ Select (queryValues  q') 
                                (queryTables  q')
                                (queryFilters q')
                                (queryOrders  q')
    
    state $ \(i,q) -> (thing, (i', q))

----------------------------------------------------------------------- Utility

emptyQuery = QueryData [] [] [] []

addTable :: String -> (Int, QueryData) -> QueryResult FieldMapper
addTable name (i, q) = 
    let existingTables = queryTables q
        newTable       = Table name i $ if null existingTables
                             then BaseTable
                             else InnerJoin Nothing
    in ( Field i
       , (i+1, q { queryTables = existingTables ++ [newTable] } ))

addOrder :: Order -> (Int, QueryData) -> QueryResult ()
addOrder order (i, q) = ((), (i, q { queryOrders = queryOrders q ++ [order] }))

addValues :: [Field] -> (Int, QueryData) -> QueryResult ()
addValues values (i, q) = ((), (i, q { queryValues = queryValues q ++ values }))

addFilter :: Filter -> (Int, QueryData) -> QueryResult ()
addFilter filter (i, q) = ((), (i, q { queryFilters = queryFilters q ++ [filter] }))

setOrder :: Order -> (Int, QueryData) -> QueryResult ()
setOrder order (i, q) = ((), (i, q { queryOrders = [order] }))
