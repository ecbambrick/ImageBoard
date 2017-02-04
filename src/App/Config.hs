{-# LANGUAGE OverloadedStrings #-}

module App.Config where

import qualified Data.Configurator as Configurator
import qualified Data.DateTime     as DateTime

import Data.Configurator    ( Worth(..) )
import Data.DateTime        ( TimeZone )
import System.FilePath      ( (</>) )

-- | Application settings.
data Config = Config
    { configPort                :: Int
    , configDatabaseConnection  :: String
    , configStoragePath         :: FilePath
    , configPageSize            :: Int
    , configThumbnailSize       :: Int
    , configTimeZone            :: TimeZone }

-- | Returns the application's settings loaded from the config file.
loadConfig :: IO Config
loadConfig = do
    config      <- Configurator.load [Required "app.cfg"]
    storagePath <- Configurator.require config "storage_path"
    port        <- Configurator.lookupDefault 8000 config "port"
    pageSize    <- Configurator.lookupDefault 50   config "page_size"
    thumbSize   <- Configurator.lookupDefault 512  config "thumbnail_size"
    timeZone    <- DateTime.getCurrentTimeZone

    let database = storagePath </> "data" </> "database.sqlite3"

    return (Config port database storagePath pageSize thumbSize timeZone)
