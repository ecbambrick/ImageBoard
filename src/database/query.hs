{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module Database.Query where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad.State ( State, execState, state, get )

------------------------------------------------------------------------- Types

-- | The Query monad which is used to generate SQL queries.
type Query = State (Int, QueryData)

-- | An expression by which to filter a query.
type Filter = Query Where

-- | A function that maps a column name to a table column.
type Table = String -> Column

-- | A result returned when modifying query data.
type QueryResult a = (a, (Int, QueryData))

-- | A mapping between a column name and a value.
data Mapping = Mapping 
    { mappingColumn :: String
    , mappingValue  :: Value 
    } deriving (Show)

-- | A SQL value.
data Value where
    SQLNumber :: (Show a, Num a) => a -> Value
    SQLString :: String -> Value
    SQLBool   :: Bool -> Value
    SQLColumn :: Column -> Value

instance Show Value where
    show (SQLNumber x) = "SQLNumber " ++ show x
    show (SQLString x) = "SQLString " ++ show x
    show (SQLBool   x) = "SQLBool "   ++ show x
    show (SQLColumn x) = "SQLObject " ++ show x

-- | A SQL table column.
data Column = AliasedColumn Int String
            | NamedColumn String String
            deriving (Show)

-- | The 'from' clause of a query.
data From = From String Int Join deriving (Show)

-- | The type of join for a table.
data Join = BaseTable | InnerJoin (Maybe Where) deriving (Show)

-- | The 'order by' clause of a query.
data OrderBy = Asc Column | Desc Column | Random deriving (Show)

-- | The 'where' clause of a query.
data Where = All
           | Not Where
           | Equals Column Value
           | Exists QueryData
           | Like Column String
           | And Where Where
           deriving (Show)

-- | The data for a query.
data QueryData = QueryData
    { querySelect  :: [Column]
    , queryFrom    :: [From]
    , queryWhere   :: [Where]
    , queryOrderBy :: [OrderBy] 
    } deriving (Show)

-- | Converts a normal value to a SQL value.
class ToValue a where toValue :: a -> Value

instance ToValue Int    where toValue = SQLNumber
instance ToValue Float  where toValue = SQLNumber
instance ToValue Double where toValue = SQLNumber
instance ToValue String where toValue = SQLString
instance ToValue Column where toValue = SQLColumn
instance ToValue Bool   where toValue = SQLBool

--------------------------------------------------------------- Query functions

-- | Selects from the table with the given name and returns a function to map
-- | the table to column names.
table :: String -> Query Table
table = state . addTable

-- | Joins the given table based on the given filter and returns the first 
-- | argument.
on :: Query Table -> (Table -> Filter) -> Query Table
on queryMapper queryFilter = do
    mapper <- queryMapper
    filter <- Just <$> queryFilter mapper
    state $ \(i,q) ->
        let ([(From name _ _)], xs) = splitAt 1 (queryFrom q)
            x                       = From name (i - 1) (InnerJoin filter)
            
        in (AliasedColumn (i-1), (i, q { queryFrom = x:xs }))

-- | Adds the given list of columns to the columns that are returned by the 
-- | query.
retrieve :: [Column] -> Query ()
retrieve = state . addValues

-- | Adds the given filter to the query.
wherever :: Filter -> Query ()
wherever = (state . addFilter =<<)

-- | Adds the given ordering to the query such that the query is ordered by the 
-- | given column in ascending order.
asc :: Column -> Query ()
asc = state . addOrder . Asc

-- | Adds the given ordering to the query such that the query is ordered by the 
-- | given column in descending order.
desc :: Column -> Query ()
desc = state . addOrder . Desc

-- | Orders the query randomly.
randomOrder :: Query ()
randomOrder = state (setOrder Random)

-------------------------------------------------------------- Filter Functions

-- | Filters out results where the value for the given column does not equal 
-- | the given value.
(.=) :: (ToValue a) => Column -> a -> Filter
(.=) column value = return $ Equals column (toValue value)

-- | Filters out results that do not satisfy both of the given filters.
(.&) :: Filter -> Filter -> Filter
(.&) filter1 filter2 = And <$> filter1 <*> filter2
infixr 2 .&

-- | Filters out results where the value for the given column does not match
-- | the given pattern.
like :: Column -> String -> Filter
like column value = return $ Like column value

-- | Filters out results where the given query does not return any results.
exists :: Query a -> Filter
exists q = do
    (i,_) <- get
    let (i', q') = execState q (i, emptyQuery)
    state $ \(_, q'') -> (Exists q', (i', q''))

-- | Does not filter an results.
anything :: Filter
anything = return All

------------------------------------------------------------- Mapping functions

-- | Maps a table column with the given name to the given value.
(<<) :: (ToValue a) => String -> a -> Mapping
(<<) column value = Mapping column (toValue value)

----------------------------------------------------------------------- Utility

-- | An empty query.
emptyQuery = QueryData [] [] [] []

-- | Adds the given filter to the given query data when called by a state 
-- | monad.
addFilter :: Where -> (Int, QueryData) -> QueryResult ()
addFilter filter (i, q) = ((), (i, q { queryWhere = x:xs }))
    where xs = queryWhere q
          x  = filter

-- | Adds the given ordering to the given query data when called by a state 
-- | monad.
addOrder :: OrderBy -> (Int, QueryData) -> QueryResult ()
addOrder order (i, q) = ((), (i, q { queryOrderBy = x:xs }))
    where xs = queryOrderBy q
          x  = order

-- | Adds the table with the given name to the given query data when called by 
-- | a state monad.
addTable :: String -> (Int, QueryData) -> QueryResult Table
addTable name (i, q) = (AliasedColumn i, (i + 1, q { queryFrom = x:xs }))
    where xs = queryFrom q
          x  = if null xs then From name i BaseTable
                          else From name i (InnerJoin Nothing)

-- | Adds the given list of values to the given query data when called by a 
-- | state monad.
addValues :: [Column] -> (Int, QueryData) -> QueryResult ()
addValues values (i, q) = ((), (i, q { querySelect = querySelect q ++ values }))

-- | Replaces the given query data's orderings with the given ordering when 
-- | called by a state monad.
setOrder :: OrderBy -> (Int, QueryData) -> QueryResult ()
setOrder order (i, q) = ((), (i, q { queryOrderBy = [order] }))
