{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Types where

import App.Config           ( Config )
import Data.DateTime        ( DateTime )
import Control.Monad.Trans  ( MonadIO )
import Control.Monad.Reader ( MonadReader )

-- | Main application monad which allows read-only access to configuration
-- | settings.
type App a = forall m . (MonadIO m, MonadReader Config m) => m a

-- | The method with which to delete a post. Posts that are marked as deleted
-- | will remain in the database and file system until they are permanently
-- | deleted.
data DeletionMode = MarkAsDeleted | PermanentlyDelete

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

-- | A page contains the meta data of a single image file within an album.
data Page = Page
    { pageNumber    :: Int
    , pageTitle     :: String
    , pageExtension :: String
    } deriving (Eq, Show)

-- | A scope contains an alias for an expression to simplify querying.
data Scope = Scope
    { scopeName       :: String
    , scopeExpression :: String
    } deriving (Eq, Show)

-- | A tag can be attached to an image or album as additional meta data.
data Tag = Tag
    { tagName :: String
    } deriving (Eq, Show)
