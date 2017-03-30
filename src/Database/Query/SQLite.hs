{-# LANGUAGE RecordWildCards   #-}

module Database.Query.SQLite ( select, insert, update, delete ) where

import Control.Monad.State ( execState )
import Data.List           ( intercalate )
import Text.Printf         ( printf )
import Database.Query

--------------------------------------------------------------- String building

-- | Conversion of values to a SQLite expression.
class ToSQLite a where
    toSQLite :: a -> String

instance ToSQLite Value where
    toSQLite (SQLNumber x) = show x
    toSQLite (SQLString x) = escape x
    toSQLite (SQLColumn x) = toSQLite x
    toSQLite (SQLBool   x) = if x then "1" else "0"

instance ToSQLite Column where
    toSQLite (AliasedColumn table column) = alias table |.| column
    toSQLite (NamedColumn    name column) = name |.| column
    toSQLite (Count                value) = "COUNT(" ++ toSQLite (toValue value) ++ ")"

instance ToSQLite OrderBy where
    toSQLite (Asc column)  = toSQLite column <+> "ASC"
    toSQLite (Desc column) = toSQLite column <+> "DESC"
    toSQLite (Random)      = "RANDOM()"

instance ToSQLite GroupBy where
    toSQLite (GroupBy column) = toSQLite column

instance ToSQLite Where where
    toSQLite (All)                       = "true"
    toSQLite (Not filter)                = "NOT" <+> toSQLite filter
    toSQLite (Exists select)             = "EXISTS (" ++ toSQLite select ++ ")"
    toSQLite (Equals column value)       = toSQLite column <=>  toSQLite value
    toSQLite (Greater column value)      = toSQLite column <>>  toSQLite value
    toSQLite (GreaterEqual column value) = toSQLite column <>=> toSQLite value
    toSQLite (Less column value)         = toSQLite column <<>  toSQLite value
    toSQLite (Like column value)         = toSQLite column <~>  liken value
    toSQLite (And x y)                   = toSQLite x      <&>  toSQLite y
    toSQLite (Or x y)                    = toSQLite x      <|>  toSQLite y

instance ToSQLite From where
    toSQLite (From name i join) = case join of
        InnerJoin (Just filter) -> "INNER JOIN" <+> name |+| alias i <!> toSQLite filter
        InnerJoin Nothing       -> "INNER JOIN" <+> name |+| alias i
        LeftJoin (Just filter)  -> "LEFT JOIN"  <+> name |+| alias i <!> toSQLite filter
        LeftJoin Nothing        -> "LEFT JOIN"  <+> name |+| alias i
        BaseTable               -> "FROM"       <+> name |+| alias i

instance ToSQLite QueryData where
    toSQLite QueryData {..} = intercalate "\n" $ filter (not . null)
        [ selectClause  queryFrom querySelect
        , fromClause    queryFrom
        , whereClause   queryWhere
        , groupByClause queryGroupBy
        , orderClause   queryOrderBy
        , limitClause   queryLimit
        , offsetClause  queryOffset ]

-- | Generates a select clause.
selectClause :: [From] -> [Column] -> String
selectClause tables columns
    | null tables  = "SELECT 1"
    | null columns = "SELECT *"
    | otherwise    = "SELECT" <+> intercalate ", " (map toSQLite columns)

-- | Generates a where clause.
whereClause :: [Where] -> String
whereClause filters
    | null filters = ""
    | otherwise    = "WHERE" <+> intercalate "\nAND " (reverseMap toSQLite filters)

-- | Generates a order by clause.
orderClause :: [OrderBy] -> String
orderClause orders
    | null orders = ""
    | otherwise   = "ORDER BY" <+> intercalate ", " (reverseMap toSQLite orders)

groupByClause :: [GroupBy] -> String
groupByClause groupBy
    | null groupBy = ""
    | otherwise    = "GROUP BY" <+> intercalate ", " (reverseMap toSQLite groupBy)

-- | Generates a from clause.
fromClause :: [From] -> String
fromClause tables = intercalate "\n" $ map toSQLite (reverse tables)

-- | Generates a limit clause.
limitClause :: Maybe Int -> String
limitClause Nothing  = ""
limitClause (Just x) = "LIMIT " ++ show x

-- | Generates an offset clause.
offsetClause :: Maybe Int -> String
offsetClause Nothing = ""
offsetClause (Just x) = "OFFSET " ++ show x

---------------------------------------------------------------------- Builders

-- | Converts the given query to a SQL string representing a select statement.
select :: Query a -> String
select query = toSQLite $ executeQuery query (0, emptyQuery)

    where executeQuery x y = snd $ execState x y

-- | Converts the given table name and list of mappings to a SQL string
-- | representing an insert statement.
insert :: String -> [Mapping] -> String
insert table mappings = printf "INSERT INTO [%s] (%s) VALUES (%s)" table columns' mappings'

    where columns'  = intercalate ", " $ map mappingColumn mappings
          mappings' = intercalate ", " $ map (toSQLite . mappingValue) mappings

-- | Converts the given table name and filter to a SQL string representing a
-- | delete statement.
delete :: String -> (Table -> Filter) -> String
delete table filter = printf "DELETE FROM [%s] WHERE %s" table filter'

    where filter' = parse $ filter (NamedColumn table)

-- | Converts the given table name, filter, and list of mappings to a SQL
-- | string representing an update statement.
update :: String -> (Table -> Filter) -> [Mapping] -> String
update table filter mappings
    | null mappings = "-- no set values given"
    | otherwise     = printf "UPDATE [%s] SET %s WHERE %s" table mappings' filter'

    where filter'   = parse $ filter (NamedColumn table)
          mappings' = intercalate ", " (map format mappings)
          format (Mapping column value) = wrap column <=> toSQLite value

----------------------------------------------------------------------- Utility

-- | Joins the two given strings with a period after wrapping them in brackets.
(|.|) :: String -> String -> String; infixr 9 |.|
(|.|) x y = wrap x ++ "." ++ wrap y

-- | Joins the two given strings with a space after wrapping them in brackets.
(|+|) :: String -> String -> String; infixr 9 |+|
(|+|) x y = wrap x ++ " " ++ wrap y

-- | Joins the two given strings with a space.
(<+>) :: String -> String -> String; infixr 8 <+>
(<+>) x y = x ++ " " ++ y

-- | Joins the two given strings with " ON ".
(<!>) :: String -> String -> String; infixr 7 <!>
(<!>) x y = x ++ " ON " ++ y

-- | Joins the two given strings with " = ".
(<=>) :: String -> String -> String
(<=>) x y = x ++ " = " ++ y

-- | Joins the two given strings with " LIKE ".
(<~>) :: String -> String -> String
(<~>) x y = x ++ " LIKE " ++ y

-- | Joins the two given strings with " > ".
(<>>) :: String -> String -> String
(<>>) x y = x ++ " > " ++ y

-- | Joins the two given strings with " >= ".
(<>=>) :: String -> String -> String
(<>=>) x y = x ++ " >= " ++ y

-- | Joins the two given strings with " < ".
(<<>) :: String -> String -> String
(<<>) x y = x ++ " < " ++ y

-- | Joins the two given strings with " AND ".
(<&>) :: String -> String -> String
(<&>) x y = x ++ " AND " ++ y

-- | Joins the two given strings with " OR ".
(<|>) :: String -> String -> String
(<|>) x y = x ++ " OR " ++ y

-- | Converts the given number to a table alias.
alias :: Int -> String
alias = ("t" ++) . show

-- | Converts the given string to a SQL string.
escape :: String -> String
escape x = "'" ++ replace '\'' "''" x ++ "'"

-- | Converts the given string to a SQL 'like' statement.
liken :: String -> String
liken x = escape x ++ " ESCAPE '\\'"

-- | Converts the given filter to a string representing a SQLite where clause.
parse :: Filter -> String
parse filter = toSQLite $ head $ queryWhere $ snd $ query
    where query = execState (wherever filter) (0, emptyQuery)

-- | Replace every occurrence of the given character with the given string.
replace :: Char -> String -> String -> String
replace _ _ [] = []
replace from to (x:xs)
    | x == from = to  ++ replace from to xs
    | otherwise = [x] ++ replace from to xs

-- | Maps the given fucntion to the reverse of the given list.
reverseMap :: (a -> b) -> [a] -> [b]
reverseMap f xs = map f (reverse xs)

-- | Wraps the given string in brackets.
wrap :: String -> String
wrap x = "[" ++ x ++ "]"
