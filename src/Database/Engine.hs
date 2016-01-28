{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Database.Engine
    ( Entity(..), ID, Transaction, Simple.FromRow, delete, fromEntity, insert
    , query, runDatabase, Simple.field, Simple.fromRow, single, update
    , execute ) where

import qualified Database.SQLite.Simple as Simple
import qualified Database.Query.SQLite  as SQL

import Control.Monad.Reader ( ReaderT, ask, runReaderT )
import Control.Monad.Trans  ( MonadIO, lift, liftIO )
import Data.Int             ( Int64 )
import Data.Maybe           ( listToMaybe )
import Data.Text            ( pack )
import Database.Query       ( Filter, Mapping, Query, Table, limit )

------------------------------------------------------------------------- Types

-- | The primary key of a database entity.
type ID = Int64

-- | Database transaction monad which allows access to an open database
-- | connection.
type Transaction = ReaderT Simple.Connection IO

-- | A database entity with an ID.
data Entity a = Entity
    { entityID   :: ID
    , entityData :: a
    } deriving (Eq, Show)

instance Functor Entity where
    fmap f (Entity id x) = Entity id (f x)

-- | Returns the enclosed value from an entity.
fromEntity :: Entity a -> a
fromEntity (Entity _ x) = x

-- | Runs a transaction with the given database connection string.
runDatabase :: (MonadIO m) => String -> Transaction a -> m a
runDatabase db f = liftIO $ Simple.withConnection db $ \conn -> do
    Simple.execute_ conn "PRAGMA foreign_keys = ON;"
    Simple.withTransaction conn $ do
        runReaderT f conn

----------------------------------------------------------------------- Queries

-- | Performs a SELECT query that returns a list of results.
query :: (Simple.FromRow r) => Query a -> Transaction [r]
query f = do
    conn <- ask
    lift $ Simple.query_ conn $ toQuery (SQL.select f)

-- | Performs a SELECT query that returns zero or one result.
single :: (Simple.FromRow r) => Query a -> Transaction (Maybe r)
single f = do
    conn    <- ask
    results <- lift $ Simple.query_ conn $ toQuery $ SQL.select (f >> limit 1)
    return (listToMaybe results)

-- | Performs an INSERT query on the given table with the give values and
-- | returns the newly inserted ID.
insert :: String -> [Mapping] -> Transaction ID
insert table mappings = do
    conn <- ask
    lift $ do
        Simple.execute_ conn $ toQuery (SQL.insert table mappings)
        Simple.lastInsertRowId conn

-- | Performs a DELETE query on the given table for all rows that satisfy the
-- | given filter.
delete :: String -> (Table -> Filter) -> Transaction ()
delete table filter = do
    conn <- ask
    lift $ Simple.execute_ conn $ toQuery (SQL.delete table filter)

-- | Performs an UPDATE query on the given table for all rows that satisfy the
-- | given filter.
update :: String -> (Table -> Filter) -> [Mapping] -> Transaction ()
update table filter mappings = do
    conn <- ask
    lift $ Simple.execute_ conn $ toQuery (SQL.update table filter mappings)

-- | Executes the given string as literal SQL.
execute :: String -> Transaction ()
execute q = do
    conn <- ask
    lift $ Simple.execute_ conn $ toQuery q

----------------------------------------------------------------------- Utility

-- | Converts a string to a query.
toQuery :: String -> Simple.Query
toQuery = Simple.Query . pack
