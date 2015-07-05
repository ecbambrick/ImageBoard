{-# LANGUAGE OverloadedStrings #-}

import Common               ( Config(..), App )
import Control.Applicative  ( (<$>), (<*>), (<|>), pure )
import Control.Monad.Reader ( asks, msum, runReaderT )
import Control.Monad.Trans  ( liftIO )
import Data.Configurator    ( Worth(..), load, lookupDefault, require )
import Happstack.Extended   ( get, other, post, render, root, uri )
import Happstack.Server     ( BodyPolicy, Browsing(..), Response, decodeBody
                            , defaultBodyPolicy, notFound, nullConf
                            , serveDirectory, simpleHTTP )
import Paths                ( absoluteImagesDir )
import Routing              ( index, upload )
import System.Directory     ( getTemporaryDirectory )

-- | Main.
main :: IO ()
main = simpleHTTP nullConf . runReaderT (setBody >> setRoutes) =<< getConfig

-- | Returns the configuration settings loaded from 'app.cfg'.
getConfig :: IO Config
getConfig = do
    config <- load [Required "app.cfg"]
    Config <$> lookupDefault 8000     config "port"
           <*> require                config "database"
           <*> require                config "storage_path"
           <*> lookupDefault 10000000 config "disk_quota"

-- | Sets the request body policy.
setBody :: App ()
setBody = decodeBody =<< defaultBodyPolicy <$> liftIO getTemporaryDirectory
                                           <*> asks configDiskQuota
                                           <*> pure 1000
                                           <*> pure 1000

-- | Sets the routes.
setRoutes :: App Response
setRoutes = do
    imagesDir <- absoluteImagesDir
    msum [ uri "images" $ serveDirectory DisableBrowsing [] imagesDir
         , uri "upload" $ post upload
         , root         $ get index
         , other        $ notFound (render "404") ]
