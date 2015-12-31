{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Image 
    ( count, delete, insert, query, querySingle, queryTriple, update ) where

import qualified App.Core.Tag as Tag

import App.Common           ( Tag(..), Image(..), App, runDB )
import App.Config           ( Config(..) )
import App.Expression       ( Expression )
import App.Database         ( attachTags, detachTags, cleanTags, deleteImage
                            , insertImage, selectHashExists, selectImagesCount
                            , selectImage, selectImages, selectNextImage
                            , selectPreviousImage, updateImage )
import App.FileType         ( ImageFile, File(..) )
import App.Paths            ( getImagePath, getImageThumbnailPath )
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
import Graphics.Thumbnail   ( createThumbnail )
import System.Directory     ( copyFile, createDirectoryIfMissing, doesFileExist
                            , removeFile )
import System.FilePath      ( takeDirectory )
import System.IO.Metadata   ( getHash, getDimensions, getSize )

------------------------------------------------------------------------- Types

-- | A tuple of three values of the same type.
type Triple a = (a, a, a)

-------------------------------------------------------------------------- CRUD

-- | Returns the total number of images satisfying the given expression.
count :: Expression -> App Int
count = runDB . selectImagesCount

-- | Deletes the image with the given ID from the database/filesystem.
delete :: ID -> App ()
delete id = do
    image <- querySingle id

    case image of 
        Just (Entity _ image) -> delete' image
        Nothing               -> return ()
    
    where 
    delete' image = do
        imagePath   <- getImagePath image
        thumbPath   <- getImageThumbnailPath image
        imageExists <- liftIO $ doesFileExist imagePath
        thumbExists <- liftIO $ doesFileExist thumbPath

        runDB $ do
            deleteImage id
            cleanTags
        
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
    (w, h)      <- liftIO $ getDimensions fromPath
    hashExists  <- runDB  $ selectHashExists hash
    thumbSize   <- asks   $ configThumbnailSize
    
    let isDuplicate = verify (not hashExists) (Error "hash" hash "duplicate hash") 
        tags        = Tag.cleanTags tagNames
        image       = Image title False hash ext w h now now size tags
        results     = validate image <> isDuplicate
                 
    when (isValid results) $ do
        toPath    <- getImagePath image
        thumbPath <- getImageThumbnailPath image
        
        runDB $ do
            insertImage image >>= attachTags tags
        
        liftIO $ do
            createDirectoryIfMissing True $ takeDirectory toPath
            copyFile fromPath toPath
            createThumbnail thumbSize toPath thumbPath
    
    return results

-- | Returns a page of images based on the given page number and filter.
query :: Expression -> Int -> App [Entity Image]
query expression page = do
    size <- asks configPageSize
    runDB $ selectImages expression ((page - 1) * size) size

-- | Returns the image with the given ID.
querySingle :: ID -> App (Maybe (Entity Image))
querySingle = runDB . selectImage

-- | Returns the image with the given ID along with the two adjacent images
-- | based on the given filter.
queryTriple :: Expression -> ID -> App (Triple (Maybe (Entity Image)))
queryTriple expression id = runDB $ do
    main <- selectImage id
    next <- selectNextImage id expression
    prev <- selectPreviousImage id expression

    return (prev, main, next)

-- | Updates the given image in the database. Returns valid if the update was
-- | successful; otherwise, invalid.
update :: Entity Image -> App Validation
update (Entity id image) = do
    now      <- liftIO $ getCurrentTime
    previous <- runDB  $ selectImage id
    
    let isFound = verify (isJust previous) (Error "id" (show id) "ID not found") 
        results = validate image <> isFound
    
    when (isValid results) $ runDB $ do
        let newTags = Tag.cleanTags $ imageTagNames image
            oldTags = imageTagNames . fromEntity . fromJust $ previous
    
        updateImage (Entity id image { imageModified = now })
        detachTags (oldTags \\ newTags) id
        attachTags (newTags \\ oldTags) id
        cleanTags
    
    return results

----------------------------------------------------------------------- Utility

-- | Returns valid if all fields of the given image are valid; otherwise 
-- | invalid. Any validation that requires access to the database is ignored.
validate :: Image -> Validation
validate Image {..} = mconcat
    [ isPositive (Property "size" imageFileSize)
    , mconcat    (Tag.validate . Tag <$> imageTagNames) ]
