{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Image where

import qualified App.Config             as Config
import qualified App.Core.Source        as Source
import qualified App.Core.Tag           as Tag
import qualified App.Core.Title         as Title
import qualified App.Storage.Database   as DB
import qualified App.Storage.FileSystem as FileSystem
import qualified App.Validation         as Validation
import qualified Data.DateTime          as DateTime
import qualified Graphics.FFmpeg        as Graphics
import qualified System.IO.Metadata     as Metadata

import App.Control          ( runDB )
import App.Core.Types       ( DeletionMode(..), Image(..), App, ID, URL )
import App.Expression       ( Expression )
import App.Validation       ( Error(..), Validation )
import Control.Monad        ( void )
import Data.List            ( (\\) )
import Data.Maybe           ( fromJust, isJust )

----------------------------------------------------------------------- Queries

-- | Returns the total number of images satisfying the given expression.
count :: Expression -> App Int
count = runDB . DB.selectImagesCount

-- | Returns a page of images satisfying the given expression.
query :: Expression -> Int -> App [Image]
query expression page = do
    size <- Config.pageSize
    runDB $ DB.selectImages expression ((page - 1) * size) size

-- | Returns the image with the given ID.
querySingle :: ID -> App (Maybe Image)
querySingle = runDB . DB.selectImage

-- | Returns the image with the given ID along with the two adjacent images,
-- | all of which must satisfy the given expression.
queryTriple :: Expression -> ID -> App (Maybe (Image, Image, Image))
queryTriple expression id = runDB $ do
    main <- DB.selectImage id
    next <- DB.selectNextImage id expression
    prev <- DB.selectPreviousImage id expression

    return $ (,,) <$> prev <*> main <*> next

---------------------------------------------------------------------- Commands

-- | Deletes the image with the given ID. If the delete is permanent, it is
-- | removed from the database/file system; otherwise, it is just marked as
-- | deleted.
delete :: DeletionMode -> ID -> App ()
delete mode imageID = do
    image <- querySingle imageID

    case (image, mode) of
        (Just _, MarkAsDeleted) -> do
            runDB $ DB.markPostAsDeleted imageID

        (Just image, PermanentlyDelete) -> do
            runDB $ do
                DB.deletePost imageID
                DB.cleanTags
            FileSystem.deleteImage image

        _ -> do
            return ()

-- | Inserts a new image into the database/filesystem based on the given file
-- | path, extension, title, sources and tags. Returns valid if the insertion
-- | was sucessful; otherwise invalid.
insert :: FilePath -> String -> String -> [URL] -> [String] -> App (Validation ())
insert path ext title urls tags = do
    now        <- DateTime.now
    thumbSize  <- Config.thumbnailSize
    hash       <- Metadata.getHash path
    size       <- Metadata.getSize path
    (w, h)     <- Graphics.getDimensions path
    hashExists <- runDB $ DB.selectHashExists hash

    let result = Image <$> pure 0
                       <*> Title.validate title
                       <*> pure False
                       <*> pure hash
                       <*> pure ext
                       <*> pure w
                       <*> pure h
                       <*> pure now
                       <*> pure now
                       <*> pure (fromIntegral size)
                       <*> Source.validateURLs urls
                       <*> Tag.validateNames tags
                       <*  Validation.reject hashExists [DuplicateHash hash]

    Validation.whenSuccess result $ \image -> do
        FileSystem.addImage image path
        runDB $ do
            imageID <- DB.insertImage image
            DB.attachTags    (imageTags image)    imageID
            DB.attachSources (imageSources image) imageID

    return (void result)

-- | Updates the given image in the database. Returns valid if the update was
-- | successful; otherwise, invalid.
update :: ID -> String -> [URL] -> [String] -> App (Validation ())
update imageID title urls tags = do
    now      <- DateTime.now
    existing <- runDB $ DB.selectImage imageID

    let result = (,,) <$> Title.validate title
                      <*> Tag.validateNames tags
                      <*> Source.validateURLs urls
                      <*  Validation.assert (isJust existing) [IDNotFound imageID]

    Validation.whenSuccess result $ \(title, tags, urls) -> do
        let newTags    = imageTags newImage
            oldTags    = imageTags oldImage
            newSources = imageSources newImage
            oldSources = imageSources oldImage
            oldImage   = fromJust existing
            newImage   = oldImage { imageTags     = tags
                                  , imageTitle    = title
                                  , imageSources  = urls
                                  , imageModified = now }

        runDB $ do
            DB.updateImage newImage
            DB.detachTags    (oldTags    \\ newTags)    imageID
            DB.attachTags    (newTags    \\ oldTags)    imageID
            DB.detachSources (oldSources \\ newSources) imageID
            DB.attachSources (newSources \\ oldSources) imageID
            DB.cleanTags

    return (void result)
