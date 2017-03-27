{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module App.Config where

import qualified Data.Configurator as Configurator
import qualified Data.DateTime     as DateTime

import Control.Monad.Reader ( MonadReader, asks )
import Data.Configurator    ( Worth(..) )
import Data.DateTime        ( TimeZone )
import System.FilePath      ( (</>) )

------------------------------------------------------------------------- Types

-- | Application settings.
data Config = Config
    { configPort                :: Int
    , configDatabaseConnection  :: String
    , configStoragePath         :: FilePath
    , configPageSize            :: Int
    , configThumbnailSize       :: Int
    , configTimeZone            :: TimeZone }

---------------------------------------------------------------- Initialization

-- | Returns the application's settings loaded from the config file.
load :: IO Config
load = do
    config      <- Configurator.load [Required "app.cfg"]
    storagePath <- Configurator.require config "storage_path"
    port        <- Configurator.lookupDefault 8000 config "port"
    pageSize    <- Configurator.lookupDefault 50   config "page_size"
    thumbSize   <- Configurator.lookupDefault 512  config "thumbnail_size"
    timeZone    <- DateTime.getCurrentTimeZone

    let database = storagePath </> "data" </> "database.sqlite3"

    return (Config port database storagePath pageSize thumbSize timeZone)

------------------------------------------------------------------------ Access

-- | Returns the configured port number.
port :: (MonadReader Config m) => m Int
port = asks configPort

-- | Returns the configured database connection string.
databaseConnection :: (MonadReader Config m) => m String
databaseConnection = asks configDatabaseConnection

-- | Returns the configured storage data path.
storagePath :: (MonadReader Config m) => m FilePath
storagePath = asks configStoragePath

-- | Returns the configured number of results per page when viewing posts.
pageSize :: (MonadReader Config m) => m Int
pageSize = asks configPageSize

-- | Returns the configured size of thumbnail to generate.
thumbnailSize :: (MonadReader Config m) => m Int
thumbnailSize = asks configThumbnailSize

-- | Returns the configured time zone for displaying time stamps.
timeZone :: (MonadReader Config m) => m TimeZone
timeZone = asks configTimeZone
