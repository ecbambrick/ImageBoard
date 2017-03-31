{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Everything where

import qualified App.Config           as Config
import qualified App.Storage.Database as Database
import qualified App.Storage.Path     as Path
import qualified System.Directory     as Dir
import qualified System.FilePath      as FilePath

import App.Core.Types       ( App )
import Control.Exception    ( ErrorCall(..), throwIO )
import Control.Monad.Reader ( liftIO, unless, when )

-- | Deletes all data from the database.
delete :: App ()
delete = do
    dataPath <- Path.dataDirectory

    liftIO $ do
        pathExists <- Dir.doesDirectoryExist dataPath
        when pathExists $ do
            Dir.removeDirectoryRecursive dataPath
            Dir.createDirectory dataPath

    initialize

-- | Initializes a new database if one does not already exist.
initialize :: App ()
initialize = do
    dataPath <- Path.dataDirectory

    liftIO $ do
        pathExists <- Dir.doesDirectoryExist dataPath
        unless pathExists $ do
            throwIO (ErrorCall "Storage path must point to an existing folder. Please verify app.cfg")

    Database.createDatabase
