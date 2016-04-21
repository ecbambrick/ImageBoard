{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Everything where

import qualified App.Database as Database
import qualified App.Path     as Path

import App.Core.Types      ( App )
import Control.Monad       ( when )
import Control.Monad.Trans ( liftIO )
import System.Directory    ( doesDirectoryExist, removeDirectoryRecursive )

-- | Deletes all data from the database.
delete :: App ()
delete = do
    dataPath   <- Path.getDataPath
    pathExists <- liftIO $ doesDirectoryExist dataPath

    liftIO $ when pathExists $ removeDirectoryRecursive dataPath

    Database.deleteDatabase
    Database.createDatabase

-- | Initializes a new database if one does not already exist.
initialize :: App ()
initialize = Database.createDatabase
