{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Common where

import App.Config           ( Config(..), loadConfig )
import Control.Monad.Reader ( MonadReader, ReaderT, ask, asks, local, runReaderT )
import Control.Monad.Trans  ( MonadIO, lift )
import Data.Time            ( UTCTime )
import Database.Engine      ( Transaction, runDatabase )
import Web.Spock            ( SpockT, ActionT, runSpock, spockT )

----------------------------------------------------------------------- Control

-- | Main application monad which allows read-only access to configuration 
-- | settings.
type App = ActionT (ReaderT Config IO)

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

-------------------------------------------------------------------------- Data

-- | A tag can be attached to an image as additional meta data.
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
    , imageCreated     :: UTCTime
    , imageModified    :: UTCTime
    , imageFileSize    :: Int
    , imageTagNames    :: [String]
    } deriving (Eq, Show)

-- | An album contains the meta data of a collection of image files that are
-- | considered an isolated group.
data Album = Album
    { albumTitle       :: String
    , albumIsFavourite :: Bool
    , albumCreated     :: UTCTime
    , albumModified    :: UTCTime
    , albumFileSize    :: Int
    , albumPages       :: [Page]
    } deriving (Show)

-- | A page contains the meta data of a single image file within an album.
data Page = Page
    { pageTitle     :: String
    , pageNumber    :: Int
    , pageExtension :: String
    } deriving (Show)

----------------------------------------------------------------------- Utility

-- | Maps a function over a nested functor.
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f2 (f1 a) -> f2 (f1 b)
(<$$>) = fmap . fmap

infixl 5 <$$>
