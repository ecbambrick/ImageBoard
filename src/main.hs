{-# LANGUAGE OverloadedStrings #-}

import qualified App.Routing as Route

import App.Common           ( Config(..), App )
import App.Paths            ( absoluteImagesDir, absoluteThumbsDir )
import Control.Applicative  ( (<$>), (<*>), (<|>), pure )
import Control.Monad.Reader ( asks, msum, runReaderT )
import Control.Monad.Trans  ( liftIO )
import Data.Configurator    ( Worth(..), load, lookupDefault, require )
import Happstack.Extended   ( get, other, post, render, root, uri )
import Happstack.Server     ( BodyPolicy, Browsing(..), Response, decodeBody
                            , defaultBodyPolicy, notFound, nullConf, path
                            , serveDirectory, simpleHTTP )
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
    thumbsDir <- absoluteThumbsDir
    msum [ uri "images"        $ serveDirectory DisableBrowsing [] imagesDir
         , uri "thumbs"        $ serveDirectory DisableBrowsing [] thumbsDir
         , uri "upload" $ post $ Route.upload
         , uri "search" $ get  $ Route.search
         , uri "image"  $ path $ Route.image
         , root         $ get  $ Route.index
         , other               $ notFound (render "404") ]
