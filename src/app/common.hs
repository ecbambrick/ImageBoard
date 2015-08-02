{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Common where

import App.Config               ( Config(..), loadConfig )
import App.Expression           ( Expression )
import Control.Monad.Reader     ( MonadReader, ReaderT, ask, asks, local
                                , runReaderT )
import Control.Monad.Trans      ( MonadIO, lift, liftIO )
import Data.Int                 ( Int64 )
import Data.Time                ( UTCTime )
import Database.SQLite.Simple   ( Connection, execute_, withConnection
                                , withTransaction )
import Web.Spock                ( SpockT, ActionT, runSpock, spockT )

----------------------------------------------------------------------- Control

-- | Main application monad which allows read-only access to configuration 
-- | settings.
type App = ActionT (ReaderT Config IO)

instance (MonadReader r m) => MonadReader r (ActionT m) where
    ask       = lift ask
    local f m = runReaderT (lift m) . f =<< ask

-- | Database transaction monad which allows access to an open database 
-- | connection.
type Transaction = ReaderT Connection IO

-- | Runs a transaction with the database connection string defined by the
-- | application's configuration settings.
runDB :: (MonadIO m, MonadReader Config m) => Transaction a -> m a
runDB f = do
    db <- asks configDatabaseConnection
    liftIO $ withConnection db $ \conn -> do
        withTransaction conn $ do
            execute_ conn "PRAGMA foreign_keys = ON;"
            runReaderT f conn

-- | Runs the application with the settings from the config file.
runApplication :: SpockT (ReaderT Config IO) () -> IO ()
runApplication routes = do
    config <- loadConfig
    runSpock (configPort config) $ spockT (flip runReaderT config) routes

-------------------------------------------------------------------------- Data

-- | The primary key of a database entity.
type ID = Int64

-- | A database entity with an ID.
data Entity a = Entity 
    { entityID   :: ID 
    , entityData :: a 
    } deriving (Eq, Show)

instance Functor Entity where
    fmap f (Entity id x) = Entity id (f x)

-- | A tag can be attached to an image as additional meta data. An ID of 
-- | nothing indicates a tag that has not yet been persisted.
data Tag = Tag 
    { tagName :: String
    } deriving (Eq, Show)

-- | An Image contains the meta data of an image file that has been uploaded. 
-- | An ID of nothing indicates an image that has not yet been persisted.
data Image = Image
    { imageTitle        :: String
    , imageIsFavourite  :: Bool
    , imageHash         :: String
    , imageExtension    :: String
    , imageWidth        :: Int
    , imageHeight       :: Int
    , imageCreated      :: UTCTime
    , imageModified     :: UTCTime
    , imageFileSize     :: Int
    , imageTagNames     :: [String]
    } deriving (Eq, Show)

----------------------------------------------------------------------- Utility

-- | Maps a function over a nested functor.
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f2 (f1 a) -> f2 (f1 b)
(<$$>) = fmap . fmap

infixl 5 <$$>

-- | Returns the enclosed value from an entity.
fromEntity :: Entity a -> a
fromEntity (Entity _ x) = x
