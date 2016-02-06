{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Image
    ( count, delete, insert, query, querySingle, queryTriple, update ) where

import qualified App.Database    as DB
import qualified Graphics.FFmpeg as Graphics
import qualified App.Path        as Path
import qualified App.Core.Tag    as Tag

import App.Common           ( DeletionMode(..), Tag(..), Image(..), App, runDB )
import App.Config           ( Config(..) )
import App.Expression       ( Expression )
import App.FileType         ( ImageFile, File(..) )
import App.Validation       ( Error(..), Property(..), Validation, isPositive
                            , isValid, verify )
import Control.Applicative  ( (<$>), (<*>) )
import Control.Monad        ( when )
import Control.Monad.Trans  ( liftIO )
import Control.Monad.Reader ( asks )
import Data.DateTime        ( getCurrentTime )
import Data.List            ( (\\) )
import Data.Maybe           ( isJust, fromJust )
import Data.Monoid          ( (<>), mconcat )
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
    (w, h)      <- liftIO $ Graphics.getDimensions fromPath
    thumbSize   <- asks   $ configThumbnailSize

    let isDuplicate = verify (not hashExists) (Error "hash" hash "duplicate hash")
        tags        = Tag.cleanTags tagNames
        image       = Image title False hash ext w h now now size tags
        results     = validate image <> isDuplicate

    when (isValid results) $ do
        toPath    <- Path.getImagePath image
        thumbPath <- Path.getImageThumbnailPath image

        runDB $ do
            DB.insertImage image >>= DB.attachTags tags

        liftIO $ do
            createDirectoryIfMissing True $ takeDirectory toPath
            copyFile fromPath toPath
            Graphics.createThumbnail thumbSize toPath thumbPath

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

    let isFound = verify (isJust previous) (Error "id" (show id) "ID not found")
        results = validate image <> isFound

    when (isValid results) $ runDB $ do
        let newTags = Tag.cleanTags $ imageTagNames image
            oldTags = imageTagNames . fromEntity . fromJust $ previous

        DB.updateImage (Entity id image { imageModified = now })
        DB.detachTags (oldTags \\ newTags) id
        DB.attachTags (newTags \\ oldTags) id
        DB.cleanTags

    return results

----------------------------------------------------------------------- Utility

-- | Returns valid if all fields of the given image are valid; otherwise
-- | invalid. Any validation that requires access to the database is ignored.
validate :: Image -> Validation
validate Image {..} = mconcat
    [ isPositive (Property "size" imageFileSize)
    , mconcat    (Tag.validate . Tag <$> imageTagNames) ]
