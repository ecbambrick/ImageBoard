{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module Database.Query where

import Control.Monad.State ( State, execState, state, gets )

------------------------------------------------------------------------- Types

-- The Query monad which is used to generate SQL queries.
type Query = State (Int, QueryData)

-- An expression by which to filter a query.
type Filter = Query Where

-- A function that maps a column name to a table column.
type Table = String -> Field

-- A result returned when modifying query data.
type QueryResult a = (a, (Int, QueryData))

-- A mapping between a column name and a value.
data Mapping = Mapping 
    { mappingColumn :: String
    , mappingValue  :: Value 
    } deriving (Show)

-- A SQL value.
data Value where
    SQLNumber  :: (Show a, Num a) => a -> Value
    SQLString  :: String -> Value
    SQLBool    :: Bool -> Value
    SQLObject  :: Field -> Value

instance Show Value where
    show (SQLNumber x) = "SQLNumber " ++ show x
    show (SQLString x) = "SQLString " ++ show x
    show (SQLBool   x) = "SQLBool "   ++ show x
    show (SQLObject x) = "SQLObject " ++ show x

-- A SQL table column.
data Field = AliasedField Int String
           | NamedField String String
           deriving (Show)

-- The 'from' clause of a query.
data From = From String Int Join deriving (Show)

-- The type of join for a table.
data Join = BaseTable | InnerJoin (Maybe Where) deriving (Show)

-- The 'order by' clause of a query.
data OrderBy = Asc Field | Desc Field | Random deriving (Show)

-- The 'where' clause of a query.
data Where = All
           | Not Where
           | Equals Field Value
           | Exists QueryData
           | Like Field String
           | And Where Where
           deriving (Show)

-- The data for a query.
data QueryData = QueryData
    { queryValues   :: [Field]
    , queryTables   :: [From]
    , queryFilters  :: [Where]
    , queryOrders   :: [OrderBy] 
    } deriving (Show)

-- Converts a normal value to a SQL value.
class ToValue a where toValue :: a -> Value

instance ToValue Int    where toValue = SQLNumber
instance ToValue Float  where toValue = SQLNumber
instance ToValue Double where toValue = SQLNumber
instance ToValue String where toValue = SQLString
instance ToValue Field  where toValue = SQLObject
instance ToValue Bool   where toValue = SQLBool

------------------------------------------------------------- Mapping functions

-- Maps a table column with the given name to the given value.
(.<-) :: (ToValue a) => String -> a -> Mapping
(.<-) field val = Mapping field (toValue val)

--------------------------------------------------------------- Query Functions

-- Selects from the table with the given name and returns a function to map
-- the table to column names.
table :: String -> Query Table
table = state . addTable

-- Joins the given table based on the given filter and returns the table 
-- mapping function.
on :: Query Table -> (Table -> Filter) -> Query Table
on mapper filter = do
    mapper' <- mapper
    filter' <- fmap Just (filter mapper')
    state $ \(i,q) ->
        let prevTables = init (queryTables q)
            lastTable  = last (queryTables q)
            name       = tableName lastTable
            nextTable  = [From name (i-1) (InnerJoin filter')]
            
        in ( AliasedField (i-1)
           , (i, q { queryTables = prevTables ++ nextTable } ))
    where tableName (From name _ _) = name

-- Selects only the given list of columns for the query.
retrieve :: [Field] -> Query ()
retrieve = state . addValues

-- Adds the given filter to the query.
wherever :: Filter -> Query ()
wherever = (>>= state . addFilter)

-- Orders the query by the given column in ascending order.
asc :: Field -> Query ()
asc = state . addOrder . Asc

-- Orders the query by the given column in descending order.
desc :: Field -> Query ()
desc = state . addOrder . Desc

-- Orders the query randomly.
randomOrder :: Query ()
randomOrder = state (setOrder Random)

-------------------------------------------------------------- Filter Functions

-- Filters out results where the given field does not equal the given value.
(.=) :: (ToValue a) => Field -> a -> Filter
(.=) field val = return $ Equals field (toValue val)

-- Filters out results that do not satisfy both of the given filters.
infixr 2 .&
(.&) :: Filter -> Filter -> Filter
(.&) f1 f2 = do
    f1' <- f1
    f2' <- f2
    return (And f1' f2') 

-- Filters out results where the given field is not like the given value where
-- the value is a SQL 'like' string.
like :: Field -> String -> Filter
like field val = return $ Like field val

-- Filters out results where the given query does not return any results.
exists :: Query a -> Filter
exists q = do
    i  <- gets fst
    
    let (i', q') = execState q (i, emptyQuery)
    
    let thing = Exists $ QueryData (queryValues  q') 
                                   (queryTables  q')
                                   (queryFilters q')
                                   (queryOrders  q')
    
    state $ \(i,q) -> (thing, (i', q))

-- Does not filter an results.
anything :: Filter
anything = return All

----------------------------------------------------------------------- Utility

-- An empty query.
emptyQuery = QueryData [] [] [] []

-- Gets the name of the given table .
tableName :: From -> String
tableName (From name _ _) = name

-- Adds the table with the given name to the given query data.
addTable :: String -> (Int, QueryData) -> QueryResult Table
addTable name (i, q) = 
    let existingTables = queryTables q
        newTable       = From name i $ if null existingTables
                             then BaseTable
                             else InnerJoin Nothing
    in ( AliasedField i
       , (i+1, q { queryTables = existingTables ++ [newTable] } ))

-- Adds the given ordering to the given query data.
addOrder :: OrderBy -> (Int, QueryData) -> QueryResult ()
addOrder order (i, q) = ((), (i, q { queryOrders = queryOrders q ++ [order] }))

-- Adds the given list of values to the given query data.
addValues :: [Field] -> (Int, QueryData) -> QueryResult ()
addValues values (i, q) = ((), (i, q { queryValues = queryValues q ++ values }))

-- Adds the given filter to the given query data.
addFilter :: Where -> (Int, QueryData) -> QueryResult ()
addFilter filter (i, q) = ((), (i, q { queryFilters = queryFilters q ++ [filter] }))

-- Replaces the given query data's orderings with the given ordering.
setOrder :: OrderBy -> (Int, QueryData) -> QueryResult ()
setOrder order (i, q) = ((), (i, q { queryOrders = [order] }))
