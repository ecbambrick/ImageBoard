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
    toSQLite (Field table field) = "[" ++ alias table ++ "].[" ++ field ++ "]"

instance ToSQLite Order where
    toSQLite (Asc field)  = unwords [toSQLite field, "ASC"]
    toSQLite (Desc field) = unwords [toSQLite field, "DESC"]
    toSQLite (Random)     = "RANDOM()"

instance ToSQLite Filter where
    toSQLite (Not filter)         = unwords ["NOT", toSQLite filter]
    toSQLite (Equals field value) = unwords [toSQLite field, "=", toSQLite value]
    toSQLite (Like field value)   = unwords [toSQLite field, "LIKE", toSQLite $ Likeness value]
    toSQLite (Exists select)      = unwords ["EXISTS (", toSQLite select, ")"]
    toSQLite (And x y)            = unwords [toSQLite x, "AND", toSQLite y]

instance ToSQLite Table where
    toSQLite (Table name i BaseTable)          = unwords ["FROM", name, alias i]
    toSQLite (Table name i (InnerJoin filter)) = unwords ["INNER JOIN", name, alias i, parse filter]
        where parse x = case x of
                          Nothing -> ""
                          Just x  -> unwords ["ON", toSQLite x]

instance ToSQLite Select where
    toSQLite Select {..} = intercalate "\n" $ filter (not . null)
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

select :: Query a -> String
select q = toSQLite $ Select values tables filters orders
    where (_, q') = execState q (0, emptyQuery)
          values  = (queryValues  q') 
          tables  = (queryTables  q')
          filters = (queryFilters q')
          orders  = (queryOrders  q')

----------------------------------------------------------------------- Utility

replace :: Char -> String -> String -> String
replace _ _ [] = []
replace from to (x:xs)
    | x == from = to  ++ replace from to xs
    | otherwise = [x] ++ replace from to xs

escape :: String -> String
escape x = "'" ++ replace '\'' "''" x ++ "'"

alias :: Int -> String
alias = ("t" ++) . show
