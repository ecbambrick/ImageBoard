{-# LANGUAGE OverloadedStrings #-}

module App.Config where

import Control.Applicative  ( (<$>), (<*>) )
import Data.Configurator    ( Worth(..), load, lookupDefault, require )
import Data.DateTime        ( TimeZone, getCurrentTimeZone )

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
    config <- load [Required "app.cfg"]
    Config <$> lookupDefault 8000 config "port"
           <*> require            config "database"
           <*> require            config "storage_path"
           <*> require            config "page_size"
           <*> require            config "thumbnail_size"
           <*> getCurrentTimeZone
