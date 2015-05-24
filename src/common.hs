{-# LANGUAGE DeriveDataTypeable #-}
module Common where

import Happstack.Server     (ServerPartT)
import Control.Monad.Reader (ReaderT)
import Control.Applicative  ((<$>), (<*>))
import Data.Data            (Data, Typeable)
import Data.Int             (Int64)

----------------------------------------------------------------------- Control

-- | Main application monad which allows read-only access to configuration 
-- | settings.
type App = ReaderT Config (ServerPartT IO)

-- | Configuration settings.
data Config = Config 
    { configPort                :: Int
    , configDatabaseConnection  :: String
    , configStoragePath         :: FilePath
    , configDiskQuota           :: Int64 }

-------------------------------------------------------------------------- Data

-- | The primary key of a database entity.
type ID = Int64

-- | The date and time as a string.
type DateTime = String

-- | A database entity with an ID.
data Entity a = Entity ID a deriving (Eq, Show)

instance Functor Entity where
    fmap f (Entity id x) = Entity id (f x)

-- | A tag can be attached to an image as additional meta data. An ID of 
-- | nothing indicates a tag that has not yet been persisted.
data Tag = Tag 
    { tagName :: String } deriving (Eq, Show, Data, Typeable)

-- | An Image contains the meta data of an image file that has been uploaded. 
-- | An ID of nothing indicates an image that has not yet been persisted.
data Image = Image
    { imageTitle        :: String
    , imageIsFavourite  :: Bool
    , imageHash         :: String
    , imageExtension    :: String
    , imageWidth        :: Int
    , imageHeight       :: Int
    , imageCreated      :: DateTime
    , imageModified     :: DateTime
    , imageFileSize     :: Int } deriving (Eq, Show, Data, Typeable)

----------------------------------------------------------------------- Utility

-- | Maps a function over a nested functor.
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f2 (f1 a) -> f2 (f1 b)
(<$$>) = fmap . fmap

-- | Returns the enclosed type from an entity.
fromEntity :: Entity a -> a
fromEntity (Entity _ x) = x
