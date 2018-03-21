{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module App.Core.Types where

import qualified Data.Aeson as JSON
import qualified Data.Text  as Text

import App.Config           ( Config )
import Data.Aeson           ( ToJSON(..), (.=) )
import Data.DateTime        ( DateTime )
import Data.Int             ( Int64 )
import Control.Monad.Trans  ( MonadIO )
import Control.Monad.Reader ( MonadReader )

-- | Main application monad which allows read-only access to configuration
-- | settings and arbitrary IO.
type App a = forall m . (MonadIO m, MonadReader Config m) => m a

-- | The ID of a persisted entity.
type ID = Int64

-- | A source URL.
type URL = String

-- | The method with which to delete a post. Posts that are marked as deleted
-- | will remain in the database and file system until they are permanently
-- | deleted.
data DeletionMode = MarkAsDeleted | PermanentlyDelete

-- | Meta data regarding a stand-alone collection of image files that has been
-- | saved.
data Album = Album
    { albumID          :: ID
    , albumTitle       :: String
    , albumIsFavourite :: Bool
    , albumCreated     :: DateTime
    , albumModified    :: DateTime
    , albumFileSize    :: Int
    , albumPages       :: [Page]
    , albumSources     :: [URL]
    , albumTags        :: [String]
    } deriving (Eq, Show)

-- | Meta data regarding an image file that has been saved.
data Image = Image
    { imageID          :: ID
    , imageTitle       :: String
    , imageIsFavourite :: Bool
    , imageHash        :: String
    , imageExtension   :: String
    , imageWidth       :: Int
    , imageHeight      :: Int
    , imageCreated     :: DateTime
    , imageModified    :: DateTime
    , imageFileSize    :: Int
    , imageSources     :: [URL]
    , imageTags        :: [String]
    } deriving (Eq, Show)

-- | Meta data regarding an image file within an album.
data Page = Page
    { pageNumber    :: Int
    , pageTitle     :: String
    , pageExtension :: String
    } deriving (Eq, Show)

-- | An alias for an expression for making frequent queries easier to use.
data Scope = Scope
    { scopeName       :: String
    , scopeExpression :: String
    } deriving (Eq, Show)

-- | A tag that has been attached to an image or album.
data SimpleTag = SimpleTag
    { tagID      :: ID
    , tagName    :: String
    , tagCreated :: DateTime
    } deriving (Eq, Show)

-- | Meta data regarding a tag.
data DetailedTag = DetailedTag
    { detailedTagID         :: ID
    , detailedTagName       :: String
    , detailedTagCreated    :: DateTime
    , detailedTagImageCount :: Int
    , detailedTagAlbumCount :: Int
    , detailedTagSample     :: Either Image Album
    , detailedTagCategories :: [String]
    } deriving (Show, Eq)

---------------------------------------------------------------- JSON instances

instance ToJSON Image where
    toJSON Image {..} = JSON.object
        [ "id"          .= imageID
        , "title"       .= Text.pack imageTitle
        , "isFavourite" .= imageIsFavourite
        , "hash"        .= Text.pack imageHash
        , "extension"   .= Text.pack imageExtension
        , "width"       .= imageWidth
        , "height"      .= imageHeight
        , "created"     .= imageCreated
        , "modified"    .= imageModified
        , "fileSize"    .= imageFileSize
        , "sources"     .= map Text.pack imageSources
        , "tags"        .= map Text.pack imageTags ]

instance ToJSON Album where
    toJSON Album {..} = JSON.object
        [ "id"          .= albumID
        , "title"       .= Text.pack albumTitle
        , "isFavourite" .= albumIsFavourite
        , "created"     .= albumCreated
        , "modified"    .= albumModified
        , "fileSize"    .= albumFileSize
        , "sources"     .= map Text.pack albumSources
        , "tags"        .= map Text.pack albumTags
        , "pages"       .= map toJSON albumPages ]

instance ToJSON Page where
    toJSON Page {..} = JSON.object
        [ "number"      .= pageNumber
        , "title"       .= pageTitle
        , "extension"   .= pageExtension ]
