{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Image
    ( count, delete, insert, query, querySingle, queryTriple, update ) where

import qualified App.Database    as DB
import qualified App.Core.Tag    as Tag
import qualified App.Path        as Path
import qualified App.Validation  as Validation
import qualified Graphics.FFmpeg as Graphics

import App.Config           ( Config(..) )
import App.Control          ( runDB )
import App.Core.Types       ( DeletionMode(..), Tag(..), Image(..), App )
import App.Expression       ( Expression )
import App.FileType         ( ImageFile, File(..) )
import App.Validation       ( Error(..), Validation )
import Control.Applicative  ( (<$>), (<*>) )
import Control.Monad        ( when )
import Control.Monad.Trans  ( liftIO )
import Control.Monad.Reader ( asks )
import Data.DateTime        ( getCurrentTime )
import Data.List            ( (\\) )
import Data.Maybe           ( isJust, fromJust )
import Data.Monoid          ( (<>), mconcat )
import Data.Textual         ( trim )
import Database.Engine      ( Entity(..), ID, fromEntity )
import System.Directory     ( copyFile, createDirectoryIfMissing, doesFileExist
                            , removeFile )
import System.FilePath      ( takeDirectory )
import System.IO.Metadata   ( getHash, getSize )

------------------------------------------------------------------------- Types

-- | A tuple of three values of the same type.
type Triple a = (a, a, a)

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

        Just (Entity _ image) -> do
            imagePath   <- Path.getImagePath image
            thumbPath   <- Path.getImageThumbnailPath image
            imageExists <- liftIO $ doesFileExist imagePath
            thumbExists <- liftIO $ doesFileExist thumbPath

            runDB $ do
                DB.deletePost id
                DB.cleanTags

            liftIO $ do
                when imageExists (removeFile imagePath)
                when thumbExists (removeFile thumbPath)

-- | Inserts a new image into the database/filesystem based on the given file,
-- | title and tags. Returns valid if the insertion was sucessful; otherwise
-- | invalid.
insert :: ImageFile -> String -> [String] -> App Validation
insert file title tagNames = do
    let fromPath = getPath file
        ext      = getExtension file

    hash        <- liftIO $ getHash fromPath
    now         <- liftIO $ getCurrentTime
    size        <- liftIO $ fromIntegral <$> getSize fromPath
    hashExists  <- runDB  $ DB.selectHashExists hash
    thumbSize   <- asks   $ configThumbnailSize

    let tags        = Tag.cleanTags tagNames
        image       = Image (trim title) False hash ext 0 0 now now size tags
        isDuplicate = Validation.verify (not hashExists) (DuplicateHash hash)
        results     = validate image <> isDuplicate

    when (Validation.isValid results) $ do
        toPath    <- Path.getImagePath image
        thumbPath <- Path.getImageThumbnailPath image

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
query :: Expression -> Int -> App [Entity Image]
query expression page = do
    size <- asks configPageSize
    runDB $ DB.selectImages expression ((page - 1) * size) size

-- | Returns the image with the given ID.
querySingle :: ID -> App (Maybe (Entity Image))
querySingle = runDB . DB.selectImage

-- | Returns the image with the given ID along with the two adjacent images
-- | based on the given filter.
queryTriple :: Expression -> ID -> App (Triple (Maybe (Entity Image)))
queryTriple expression id = runDB $ do
    main <- DB.selectImage id
    next <- DB.selectNextImage id expression
    prev <- DB.selectPreviousImage id expression

    return (prev, main, next)

-- | Updates the given image in the database. Returns valid if the update was
-- | successful; otherwise, invalid.
update :: Entity Image -> App Validation
update (Entity id image) = do
    now      <- liftIO $ getCurrentTime
    previous <- runDB  $ DB.selectImage id

    let isFound    = Validation.verify (isJust previous) (IDNotFound id)
        cleanImage = image { imageTagNames = Tag.cleanTags (imageTagNames image) }
        results    = validate cleanImage <> isFound

    when (Validation.isValid results) $
        runDB $ do
            let newTags = imageTagNames $ cleanImage
                oldTags = imageTagNames $ fromEntity $ fromJust previous

            DB.updateImage (Entity id image { imageModified = now })
            DB.detachTags (oldTags \\ newTags) id
            DB.attachTags (newTags \\ oldTags) id
            DB.cleanTags

    return results

----------------------------------------------------------------------- Utility

-- | Returns valid if all fields of the given image are valid; otherwise
-- | invalid.
validate :: Image -> Validation
validate Image {..} =
    Validation.validate
        [ Tag.validateMany $ map Tag imageTagNames
        , Validation.verify (imageFileSize > 0) (InvalidFileSize imageFileSize) ]
