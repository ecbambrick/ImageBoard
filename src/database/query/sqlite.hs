{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Query.SQLite where

import Control.Monad.State ( execState )
import Data.List           ( intercalate )
import Database.Query

--------------------------------------------------------------- String building

class ToSQLite a where
    toSQLite :: a -> String

instance ToSQLite Value where
    toSQLite (SQLNumber x) = show x
    toSQLite (SQLString x) = escape x
    toSQLite (SQLColumn x) = toSQLite x
    toSQLite (SQLBool   x) = if x then "1" else "0"

instance ToSQLite Column where
    toSQLite (AliasedColumn table column) = "[" ++ alias table ++ "].[" ++ column ++ "]"
    toSQLite (NamedColumn    name column) = "[" ++ name ++ "].[" ++ column ++ "]"

instance ToSQLite OrderBy where
    toSQLite (Asc column)  = unwords [toSQLite column, "ASC"]
    toSQLite (Desc column) = unwords [toSQLite column, "DESC"]
    toSQLite (Random)      = "RANDOM()"

instance ToSQLite Where where
    toSQLite (All)                 = "1 = 1"
    toSQLite (Not filter)          = unwords ["NOT", toSQLite filter]
    toSQLite (Equals column value) = unwords [toSQLite column, "=", toSQLite value]
    toSQLite (Like column value)   = unwords [toSQLite column, "LIKE", escape value, "ESCAPE '\\'"]
    toSQLite (Exists select)       = concat  ["EXISTS (", toSQLite select, ")"]
    toSQLite (And x y)             = unwords [toSQLite x, "AND", toSQLite y]

instance ToSQLite From where
    toSQLite (From name i BaseTable)          = unwords ["FROM", "["++name++"]", "["++alias i++"]"]
    toSQLite (From name i (InnerJoin filter)) = unwords ["INNER JOIN", "["++name++"]", "["++alias i++"]", parse filter]
        where parse x = case x of
                          Nothing -> ""
                          Just x  -> unwords ["ON", toSQLite x]

instance ToSQLite QueryData where
    toSQLite QueryData {..} = unlines $ filter (not . null)
        [ "SELECT " ++ selectClause
        , fromClause
        , whereClause
        , orderClause ]
        
        where 
        
            selectClause
                | null queryTables = "1"
                | null queryValues = "*"
                | otherwise         = intercalate ", " $ map toSQLite queryValues
                where selectAll (From name i _) = "f" ++ show i ++ ".*"
        
            fromClause = intercalate "\n" $ map toSQLite (reverse queryTables)
            
            whereClause = if null queryFilters
                then ""
                else "WHERE " ++ (intercalate "\nAND " $ map toSQLite (reverse queryFilters))
            
            orderClause = if null queryOrders
                then ""
                else "ORDER BY " ++ intercalate ", " (map toSQLite (reverse queryOrders))

---------------------------------------------------------------------- Builders

select :: Query a -> String
select q = toSQLite $ snd $ execState q (0, emptyQuery)

insert :: String -> [Mapping] -> String
insert table values = concat ["INSERT INTO [", table, "] (", a, ") VALUES (", b, ")"]
    where a = intercalate ", " $ map mappingColumn values
          b = intercalate ", " $ map (toSQLite . mappingValue) values

update :: String -> (Table -> Filter) -> [Mapping] -> String
update _ _ [] = "-- no set values given"
update table filter values = concat ["UPDATE [", table, "] SET ", intercalate ", " $ map f values, " WHERE ", toSQLite filter']
    where filter' = parseFilter (filter (NamedColumn table))
          f (Mapping column value) = "[" ++ column ++ "] = " ++ toSQLite value

delete :: String -> (Table -> Filter) -> String
delete table filter = concat ["DELETE FROM [", table, "]", " WHERE ", toSQLite filter']
    where filter' = parseFilter (filter (NamedColumn table))

----------------------------------------------------------------------- Utility

parseFilter :: Filter -> Where
parseFilter x = head $ queryFilters $ snd $ execState (wherever x) (0, emptyQuery)

replace :: Char -> String -> String -> String
replace _ _ [] = []
replace from to (x:xs)
    | x == from = to  ++ replace from to xs
    | otherwise = [x] ++ replace from to xs

escape :: String -> String
escape x = "'" ++ replace '\'' "''" x ++ "'"

alias :: Int -> String
alias = ("t" ++) . show
