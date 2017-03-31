{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Image
    ( count, delete, insert, query, querySingle, queryTriple, update ) where

import qualified App.Config           as Config
import qualified App.Core.Tag         as Tag
import qualified App.Storage.Database as DB
import qualified App.Storage.Path     as Path
import qualified App.Validation       as Validation
import qualified Data.DateTime        as DateTime
import qualified Graphics.FFmpeg      as Graphics

import App.Control          ( runDB )
import App.Core.Types       ( DeletionMode(..), Image(..), App, ID )
import App.Expression       ( Expression )
import App.Validation       ( Error(..), Validation )
import Control.Monad        ( when )
import Control.Monad.Trans  ( liftIO )
import Data.List            ( (\\) )
import Data.Monoid          ( (<>) )
import Data.Textual         ( trim )
import System.Directory     ( copyFile, createDirectoryIfMissing, doesFileExist
                            , removeFile )
import System.FilePath      ( takeDirectory )
import System.IO.Metadata   ( getHash, getSize )

-------------------------------------------------------------------------- CRUD

-- | Returns the total number of images satisfying the given expression.
count :: Expression -> App Int
count = runDB . DB.selectImagesCount

-- | Deletes the image with the given ID. If the delete is permanent, it is
-- | removed from the database/file system; otherwise, it is just marked as
-- | deleted.
delete :: DeletionMode -> ID -> App ()
delete MarkAsDeleted id = runDB (DB.markPostAsDeleted id)
delete PermanentlyDelete id = do
    image <- querySingle id

    case image of
        Nothing -> do
            return ()

        Just image -> do
            imagePath   <- Path.imageFile image
            thumbPath   <- Path.imageThumb image
            imageExists <- liftIO $ doesFileExist imagePath
            thumbExists <- liftIO $ doesFileExist thumbPath

            runDB $ do
                DB.deletePost id
                DB.cleanTags

            liftIO $ do
                when imageExists (removeFile imagePath)
                when thumbExists (removeFile thumbPath)

-- | Inserts a new image into the database/filesystem based on the given file
-- | path, extension, title and tags. Returns valid if the insertion was
-- | sucessful; otherwise invalid.
insert :: FilePath -> String -> String -> [String] -> App Validation
insert fromPath ext title tagNames = do
    now         <- DateTime.now
    thumbSize   <- Config.thumbnailSize
    hash        <- liftIO $ getHash fromPath
    size        <- liftIO $ fromIntegral <$> getSize fromPath
    hashExists  <- runDB  $ DB.selectHashExists hash

    let tags        = Tag.cleanTags tagNames
        image       = Image 0 (trim title) False hash ext 0 0 now now size tags
        isDuplicate = Validation.verify (not hashExists) (DuplicateHash hash)
        results     = validate image <> isDuplicate

    when (Validation.isValid results) $ do
        toPath    <- Path.imageFile image
        thumbPath <- Path.imageThumb image

        liftIO $ do
            createDirectoryIfMissing True $ takeDirectory toPath
            copyFile fromPath toPath
            Graphics.createThumbnail thumbSize toPath thumbPath

        (w, h) <- liftIO $ Graphics.getDimensions toPath

        runDB $ do
            let imageWithDimensions = image { imageWidth = w, imageHeight = h }
            DB.insertImage imageWithDimensions >>= DB.attachTags tags

    return results

-- | Returns a page of images based on the given page number and filter.
query :: Expression -> Int -> App [Image]
query expression page = do
    size <- Config.pageSize
    runDB $ DB.selectImages expression ((page - 1) * size) size

-- | Returns the image with the given ID.
querySingle :: ID -> App (Maybe Image)
querySingle = runDB . DB.selectImage

-- | Returns the image with the given ID along with the two adjacent images
-- | based on the given filter.
queryTriple :: Expression -> ID -> App (Maybe (Image, Image, Image))
queryTriple expression id = runDB $ do
    main <- DB.selectImage id
    next <- DB.selectNextImage id expression
    prev <- DB.selectPreviousImage id expression

    return $ (,,) <$> prev <*> main <*> next

-- | Updates the given image in the database. Returns valid if the update was
-- | successful; otherwise, invalid.
update :: ID -> String -> [String] -> App Validation
update id title tags = do
    now      <- DateTime.now
    previous <- runDB $ DB.selectImage id

    case previous of
        Just previousImage -> do
            let result   = validate newImage
                newImage = previousImage
                    { imageTags = Tag.cleanTags tags
                    , imageTitle    = trim title
                    , imageModified = now }

            when (Validation.isValid result) $
                runDB $ do
                    let newTags = imageTags newImage
                        oldTags = imageTags previousImage

                    DB.updateImage newImage
                    DB.detachTags (oldTags \\ newTags) id
                    DB.attachTags (newTags \\ oldTags) id
                    DB.cleanTags

            return result

        Nothing -> do
            return (Validation.invalidate (IDNotFound id))

----------------------------------------------------------------------- Utility

-- | Returns valid if all fields of the given image are valid; otherwise
-- | invalid.
validate :: Image -> Validation
validate Image {..} =
    Validation.validate
        [ Tag.validateMany $ imageTags
        , Validation.verify (imageFileSize > 0) (InvalidFileSize imageFileSize) ]
