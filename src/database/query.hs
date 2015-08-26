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
data Mapping = Mapping String Value deriving (Show)

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

(.<-) :: (ToValue a) => String -> a -> Mapping
(.<-) field val = Mapping field (toValue val)

--------------------------------------------------------------- Query Functions

table :: String -> Query Table
table = state . addTable

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
    
get' :: [Field] -> Query ()
get' = state . addValues

wherever :: Filter -> Query ()
wherever = (>>= state . addFilter)

asc :: Field -> Query ()
asc = state . addOrder . Asc

desc :: Field -> Query ()
desc = state . addOrder . Desc

randomOrder :: Query ()
randomOrder = state (setOrder Random)

-------------------------------------------------------------- Filter Functions

(.=) :: (ToValue a) => Field -> a -> Filter
(.=) field val = return $ Equals field (toValue val)

infixr 2 .&
(.&) :: Filter -> Filter -> Filter
(.&) f1 f2 = do
    f1' <- f1
    f2' <- f2
    return (And f1' f2') 

like :: Field -> String -> Filter
like field val = return $ Like field val

exists :: Query a -> Filter
exists q = do
    i  <- gets fst
    
    let (i', q') = execState q (i, emptyQuery)
    
    let thing = Exists $ QueryData (queryValues  q') 
                                   (queryTables  q')
                                   (queryFilters q')
                                   (queryOrders  q')
    
    state $ \(i,q) -> (thing, (i', q))

anything :: Filter
anything = return All

----------------------------------------------------------------------- Utility

emptyQuery = QueryData [] [] [] []

mappingField (Mapping field _) = field

mappingValue (Mapping _ value) = value

addTable :: String -> (Int, QueryData) -> QueryResult Table
addTable name (i, q) = 
    let existingTables = queryTables q
        newTable       = From name i $ if null existingTables
                             then BaseTable
                             else InnerJoin Nothing
    in ( AliasedField i
       , (i+1, q { queryTables = existingTables ++ [newTable] } ))

addOrder :: OrderBy -> (Int, QueryData) -> QueryResult ()
addOrder order (i, q) = ((), (i, q { queryOrders = queryOrders q ++ [order] }))

addValues :: [Field] -> (Int, QueryData) -> QueryResult ()
addValues values (i, q) = ((), (i, q { queryValues = queryValues q ++ values }))

addFilter :: Where -> (Int, QueryData) -> QueryResult ()
addFilter filter (i, q) = ((), (i, q { queryFilters = queryFilters q ++ [filter] }))

setOrder :: OrderBy -> (Int, QueryData) -> QueryResult ()
setOrder order (i, q) = ((), (i, q { queryOrders = [order] }))
