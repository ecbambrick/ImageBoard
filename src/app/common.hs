{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}

module App.Common where

import App.Expression           ( Expression )
import Control.Applicative      ( (<$>), (<*>) )
import Control.Monad.Reader     ( MonadReader, ReaderT, ask, asks, runReaderT )
import Control.Monad.Trans      ( MonadIO, liftIO )
import Data.Data                ( Data, Typeable )
import Data.Int                 ( Int64 )
import Data.Time                ( UTCTime )
import Database.SQLite.Simple   ( Connection, execute_, withConnection
                                , withTransaction )
import Happstack.Server         ( ServerPartT )

----------------------------------------------------------------------- Control

-- | Main application monad which allows read-only access to configuration 
-- | settings.
type App = ReaderT Config (ServerPartT IO)

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

-------------------------------------------------------------------------- Data

-- | The primary key of a database entity.
type ID = Int64

-- | The criteria with which to fetch entities.
data Criteria = All | Filtered Expression

-- | Configuration settings.
data Config = Config 
    { configPort                :: Int
    , configDatabaseConnection  :: String
    , configStoragePath         :: FilePath
    , configDiskQuota           :: Int64 }

-- | A database entity with an ID.
data Entity a = Entity 
    { entityID   :: ID 
    , entityData :: a 
    } deriving (Eq, Show, Data, Typeable)

instance Functor Entity where
    fmap f (Entity id x) = Entity id (f x)

-- | A tag can be attached to an image as additional meta data. An ID of 
-- | nothing indicates a tag that has not yet been persisted.
data Tag = Tag 
    { tagName :: String
    } deriving (Eq, Show, Data, Typeable)

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
    } deriving (Eq, Show, Data, Typeable)

----------------------------------------------------------------------- Utility

-- | Maps a function over a nested functor.
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f2 (f1 a) -> f2 (f1 b)
(<$$>) = fmap . fmap

-- | Returns the enclosed type from an entity.
fromEntity :: Entity a -> a
fromEntity (Entity _ x) = x
