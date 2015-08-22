{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Query.SQlite where

import Control.Monad.State ( execState )
import Data.List           ( intercalate )
import Database.Query

--------------------------------------------------------------- String Building

class ToSQLite a where
    toSQLite :: a -> String 

instance ToSQLite Likeness where
    toSQLite (Likeness x) = unwords [escape x, "ESCAPE '\\'"]

instance ToSQLite Value where
    toSQLite (SQLInt  x) = show x
    toSQLite (SQLStr  x) = escape x
    toSQLite (SQLObj  x) = toSQLite x
    toSQLite (SQLBool x) = if x then "1" else "0"

instance ToSQLite Field where
    toSQLite (AliasedField table field) = "[" ++ alias table ++ "].[" ++ field ++ "]"
    toSQLite (NamedField    name field) = "[" ++ name ++ "].[" ++ field ++ "]"

instance ToSQLite Order where
    toSQLite (Asc field)  = unwords [toSQLite field, "ASC"]
    toSQLite (Desc field) = unwords [toSQLite field, "DESC"]
    toSQLite (Random)     = "RANDOM()"

instance ToSQLite Filter where
    toSQLite (All)                = "1 = 1"
    toSQLite (Not filter)         = unwords ["NOT", toSQLite filter]
    toSQLite (Equals field value) = unwords [toSQLite field, "=", toSQLite value]
    toSQLite (Like field value)   = unwords [toSQLite field, "LIKE", toSQLite $ Likeness value]
    toSQLite (Exists select)      = unwords ["EXISTS (", toSQLite select, ")"]
    toSQLite (And x y)            = unwords [toSQLite x, "AND", toSQLite y]

instance ToSQLite Table where
    toSQLite (Table name i BaseTable)          = unwords ["FROM", "["++name++"]", "["++alias i++"]"]
    toSQLite (Table name i (InnerJoin filter)) = unwords ["INNER JOIN", name, alias i, parse filter]
        where parse x = case x of
                          Nothing -> ""
                          Just x  -> unwords ["ON", toSQLite x]

instance ToSQLite Select where
    toSQLite Select {..} = unlines $ filter (not . null)
        [ "SELECT " ++ selectClause
        , fromClause
        , whereClause
        , orderClause ]
        
        where 
        
            selectClause
                | null selectTables = "1"
                | null selectValues = "*"
                | otherwise         = intercalate ", " $ map toSQLite selectValues
                where selectAll (Table name i _) = "f" ++ show i ++ ".*"
        
            fromClause = intercalate "\n" $ map toSQLite selectTables
            
            whereClause = if null selectFilters
                then ""
                else "WHERE " ++ (intercalate "\nAND " $ map toSQLite selectFilters)
            
            orderClause = if null selectOrders
                then ""
                else "ORDER BY " ++ intercalate ", " (map toSQLite selectOrders)

---------------------------------------------------------------------- Builders

tableName :: Table -> String
tableName (Table name _ _) = name

select :: Query a -> String
select q = toSQLite $ Select values tables filters orders
    where (_, q') = execState q (0, emptyQuery)
          values  = queryValues  q' 
          tables  = queryTables  q'
          filters = queryFilters q'
          orders  = queryOrders  q'

insert :: Name -> [Mapping] -> String
insert table values = concat ["INSERT INTO [", table, "] (", a, ") VALUES (", b, ")"]
    where a = intercalate ", " $ map mappingField values
          b = intercalate ", " $ map (toSQLite . mappingValue) values

update :: Name -> (FieldMapper -> Query Filter) -> [Mapping] -> String
update _ _ [] = "-- no set values given"
update table filter values = concat ["UPDATE [", table, "] SET ", intercalate ", " $ map f values, " WHERE ", toSQLite filter']
    where filter' = parseFilter (filter (NamedField table))
          f (Mapping field value) = "[" ++ field ++ "] = " ++ toSQLite value

delete :: Name -> (FieldMapper -> Query Filter) -> String
delete table filter = concat ["DELETE FROM [", table, "]", " WHERE ", toSQLite filter']
    where filter' = parseFilter (filter (NamedField table))

----------------------------------------------------------------------- Utility

parseFilter :: Query Filter -> Filter
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
