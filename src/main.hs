{-# LANGUAGE OverloadedStrings #-}

import Common                 ( Config(..), App(..) )
import Control.Applicative    ( (<$>), (<*>), (<|>) )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Reader   ( asks, msum, runReaderT )
import Data.Configurator      ( Worth(..), load, lookupDefault, require )
import Happstack.Extended     ( Route(..), get, post, render, url )
import Happstack.Server       ( Browsing(..), Response, decodeBody
                              , defaultBodyPolicy, notFound, nullConf
                              , serveDirectory, simpleHTTP )
import System.Directory       ( getTemporaryDirectory )

-- | Main.
main :: IO ()
main = do
    config <- getConfig
    simpleHTTP nullConf $ runReaderT serve config

-- | Returns the configuration settings loaded from 'app.cfg'.
getConfig :: IO Config
getConfig = do
    config <- load [Required "app.cfg"]
    Config <$> lookupDefault 8000     config "port"
           <*> require                config "database"
           <*> require                config "storage_path"
           <*> lookupDefault 10000000 config "disk_quota"

-- | Sets the request body policy and prepares the routes.
serve :: App Response
serve = do
    tmpDir      <- liftIO $ getTemporaryDirectory
    size        <- asks configDiskQuota
    storagePath <- asks configStoragePath
    decodeBody (defaultBodyPolicy tmpDir size 1000 1000)
    msum [ url (Path "images") $ serveDirectory DisableBrowsing [] storagePath
         , url Any             $ notFound (render "404") ]
