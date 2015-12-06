{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module App.Common where

import App.Config           ( Config(..), loadConfig )
import Control.Monad        ( when )
import Control.Monad.Reader ( MonadReader, ReaderT, ask, asks, local, runReaderT )
import Control.Monad.Trans  ( MonadIO, lift )
import Data.DateTime        ( DateTime, utcTimeZone )
import Data.Textual         ( splitOn )
import Database.Engine      ( Transaction, execute, runDatabase )
import System.Directory     ( createDirectory, doesDirectoryExist
                            , getTemporaryDirectory, removeDirectoryRecursive )
import System.FilePath      ( (</>) )
import Web.Spock            ( SpockT, ActionT, runSpock, spockT )

----------------------------------------------------------------------- Control

-- | Main application monad which allows read-only access to configuration 
-- | settings.
type App a = forall m . (MonadIO m, MonadReader Config m) => m a

instance (MonadReader r m) => MonadReader r (ActionT m) where
    ask       = lift ask
    local f m = runReaderT (lift m) . f =<< ask

instance (MonadReader r m) => MonadReader r (SpockT m) where
    ask       = lift ask
    local f m = runReaderT (lift m) . f =<< ask

-- | Runs the application with the settings from the config file.
runApplication :: SpockT (ReaderT Config IO) () -> IO ()
runApplication routes = do
    config <- loadConfig
    runSpock (configPort config) $ spockT (flip runReaderT config) routes

-- | Run a database transaction using the connection string in the application 
-- | config.
runDB :: Transaction a -> App a
runDB command = do
    db <- asks configDatabaseConnection
    runDatabase db command

-- | Runs the give application function using a blank database and a temporary 
-- | storage directory. Used for testing.
testApplication :: App a -> IO ()
testApplication f = do 
    testDir   <- fmap (</> "testing") getTemporaryDirectory
    dirExists <- doesDirectoryExist testDir
    schema    <- readFile "schema.sql"
    
    let database = testDir </> "database"
    
    when dirExists (removeDirectoryRecursive testDir)
    createDirectory testDir
    writeFile database ""
    
    runDatabase database $
        let commands = filter (/= "\n") $ splitOn ";" schema
        in  mapM_ execute commands
    
    runReaderT f (Config 8000 database testDir 100 256 utcTimeZone)
    
    removeDirectoryRecursive testDir

-------------------------------------------------------------------------- Data

-- | A tag can be attached to an image or album as additional meta data.
data Tag = Tag 
    { tagName :: String
    } deriving (Eq, Show)

-- | An Image contains the meta data of an image file that has been uploaded. 
data Image = Image
    { imageTitle       :: String
    , imageIsFavourite :: Bool
    , imageHash        :: String
    , imageExtension   :: String
    , imageWidth       :: Int
    , imageHeight      :: Int
    , imageCreated     :: DateTime
    , imageModified    :: DateTime
    , imageFileSize    :: Int
    , imageTagNames    :: [String]
    } deriving (Eq, Show)

-- | An album contains the meta data of a collection of image files that are
-- | considered an isolated group.
data Album = Album
    { albumTitle       :: String
    , albumIsFavourite :: Bool
    , albumCreated     :: DateTime
    , albumModified    :: DateTime
    , albumFileSize    :: Int
    , albumPages       :: [Page]
    , albumTagNames    :: [String]
    } deriving (Eq, Show)

-- | A page contains the meta data of a single image file within an album.
data Page = Page
    { pageTitle     :: String
    , pageNumber    :: Int
    , pageExtension :: String
    } deriving (Eq, Show)

----------------------------------------------------------------------- Utility

-- | Maps a function over a nested functor.
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f2 (f1 a) -> f2 (f1 b)
(<$$>) = fmap . fmap

infixl 5 <$$>
