{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module Database.Query where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad.State ( State, execState, state, get )
import Data.Int            ( Int64 )

------------------------------------------------------------------------- Types

-- | The query monad which is used to generate a SQL query.
type Query = State (Int, QueryData)

-- | An expression by which to filter a query.
type Filter = Query Where

-- | A function that maps a column name to a table column.
type Table = String -> Column

-- | A mapping between a column name and a value.
data Mapping = Mapping 
    { mappingColumn :: String
    , mappingValue  :: Value 
    } deriving (Show)

-- | A SQL value.
data Value where
    SQLNumber :: (Show a, Num a) => a -> Value
    SQLString :: String -> Value
    SQLColumn :: Column -> Value
    SQLBool   :: Bool   -> Value

instance Show Value where
    show (SQLNumber x) = "SQLNumber " ++ show x
    show (SQLString x) = "SQLString " ++ show x
    show (SQLBool   x) = "SQLBool "   ++ show x
    show (SQLColumn x) = "SQLObject " ++ show x

-- | A SQL table column.
data Column = AliasedColumn Int String
            | NamedColumn String String
            deriving (Show)

-- | The FROM clause of a query.
data From = From String Int Join deriving (Show)

-- | The type of join for a table.
data Join = BaseTable | InnerJoin (Maybe Where) deriving (Show)

-- | The ORDER BY clause of a query.
data OrderBy = Asc Column | Desc Column | Random deriving (Show)

-- | The WHERE clause of a query.
data Where = All
           | Not Where
           | And Where Where
           | Or Where Where
           | Exists QueryData
           | Equals Column Value
           | Greater Column Value
           | Less Column Value
           | Like Column String
           deriving (Show)

-- | The data for a query.
data QueryData = QueryData
    { querySelect  :: [Column]
    , queryFrom    :: [From]
    , queryWhere   :: [Where]
    , queryOrderBy :: [OrderBy]
    , queryOffset  :: Maybe Int
    , queryLimit   :: Maybe Int
    } deriving (Show)

-- | Converts a normal value to a SQL value.
class ToValue a where toValue :: a -> Value

instance ToValue Int     where toValue = SQLNumber
instance ToValue Int64   where toValue = SQLNumber
instance ToValue Integer where toValue = SQLNumber
instance ToValue Float   where toValue = SQLNumber
instance ToValue Double  where toValue = SQLNumber
instance ToValue String  where toValue = SQLString
instance ToValue Column  where toValue = SQLColumn
instance ToValue Bool    where toValue = SQLBool

--------------------------------------------------------------- Query functions

-- | Selects from the table with the given name and returns a function to map
-- | the table to column names.
from :: String -> Query Table
from = state . addTable

-- | Joins the given table based on the given filter and then returns the first 
-- | argument.
on :: Query Table -> (Table -> Filter) -> Query Table
on queryMapper queryFilter = do
    mapper <- queryMapper
    filter <- Just <$> queryFilter mapper
    state $ \(i,q) ->
        let ([(From name _ _)], xs) = splitAt 1 (queryFrom q)
            x                       = From name (i - 1) (InnerJoin filter)
            
        in (AliasedColumn (i-1), (i, q { queryFrom = x:xs }))

-- | Adds the given list of columns to the columns that are to be returned by 
-- | the query.
retrieve :: [Column] -> Query ()
retrieve = state . addValues

-- | Adds the given filter to the query.
wherever :: Filter -> Query ()
wherever = (state . addFilter =<<)

-- | Limits the number of results returned by query to the given amount.
limit :: Int -> Query ()
limit = state . setLimit

-- | Offsets the results returned by the query by the given amount.
offset :: Int -> Query ()
offset = state . setOffset

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
randomOrder = state $ setOrder (Just Random)

-- | Removes all orderings from the query.
clearOrder :: Query ()
clearOrder = state $ setOrder Nothing

-------------------------------------------------------------- Filter Functions

-- | Filters the query such that only results where the given value is equal
-- | to the given column are returned.
(.=) :: (ToValue a) => Column -> a -> Filter
(.=) column value = return $ Equals column (toValue value)

-- | Filters the query such that only results where the given value is greater
-- | than the given column are returned.
(.>) :: (ToValue a) => Column -> a -> Filter
(.>) column value = return $ Greater column (toValue value)

-- | Filters the query such that only results where the given value is less
-- | than the given column are returned.
(.<) :: (ToValue a) => Column -> a -> Filter
(.<) column value = return $ Less column (toValue value)

-- | Filters the query such that only results that satisfy both of the given 
-- | filters are returned.
(.&) :: Filter -> Filter -> Filter; infixr 2 .&
(.&) filter1 filter2 = And <$> filter1 <*> filter2

-- | Filters the query such that only results that satisfy either of the given 
-- | filters are returned.
(.|) :: Filter -> Filter -> Filter; infixr 3 .|
(.|) filter1 filter2 = Or <$> filter1 <*> filter2

-- | Returns a function that takes a table and then filters the query such that 
-- | only results that satisfy both of the given filters are returned. Useful
-- | as a shorthand for when not using do notation.
(*=) :: (ToValue a) => String -> a -> (Table -> Filter)
(*=) name value = \x -> x name .= value

-- | Filters the query such that only results where the given column begins 
-- | with the given string.
(~%) :: Column -> String -> Filter
(~%) column value = return $ Like column (value ++ "%")

-- | Filters the query such that only results where the given column ends 
-- | with the given string.
(%~) :: Column -> String -> Filter
(%~) column value = return $ Like column ("%" ++ value)

-- | Filters the query such that only results where the given column contains 
-- | the given string.
(%%) :: Column -> String -> Filter
(%%) column value = return $ Like column ("%" ++ value ++ "%")

-- | Filters the query such that only results where the given column matches 
-- | the given pattern.
like :: Column -> String -> Filter
like column pattern = return $ Like column pattern

-- | Filters the query such that only results that do not satisfy the given 
-- | filter are returned.
nay :: Filter -> Filter
nay filter = Not <$> filter

-- | Filters the query such that only results where the given sub-query returns 
-- | anything are returned.
exists :: Query a -> Filter
exists q = do
    (i,_) <- get
    let (i', q') = execState q (i, emptyQuery)
    state $ \(_, q'') -> (Exists q', (i', q''))

-- | Does not filter any results.
anything :: Filter
anything = return All

------------------------------------------------------------- Mapping functions

-- | Maps a table column with the given name to the given value.
(<<) :: (ToValue a) => String -> a -> Mapping
(<<) column value = Mapping column (toValue value)

----------------------------------------------------------------------- Utility

-- | A result returned when modifying query data.
type QueryResult a = (a, (Int, QueryData))

-- | An empty query.
emptyQuery = QueryData [] [] [] [] Nothing Nothing

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

-- | Sets the given query data's limit to the given amount when called by a 
-- | state monad.
setLimit :: Int -> (Int, QueryData) -> QueryResult ()
setLimit limit (i, q) = ((), (i, q { queryLimit = Just limit }))

-- | Sets the given query data's offset to the given amount when called by a
-- | state monad.
setOffset :: Int -> (Int, QueryData) -> QueryResult ()
setOffset offset (i, q) = ((), (i, q { queryOffset = Just offset }))

-- | Replaces the given query data's orderings with the given ordering when 
-- | called by a state monad.
setOrder :: Maybe OrderBy -> (Int, QueryData) -> QueryResult ()
setOrder Nothing      (i, q) = ((), (i, q { queryOrderBy = [] }))
setOrder (Just order) (i, q) = ((), (i, q { queryOrderBy = [order] }))
