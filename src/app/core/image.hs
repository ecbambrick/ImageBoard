{-# LANGUAGE RecordWildCards #-}

module App.Core.Image ( count, query, queryTriple, insert ) where

import qualified App.Core.Tag as Tag

import App.Common           ( Tag(..), Image(..), App, runDB )
import App.Config           ( Config(..) )
import App.Expression       ( Expression )
import App.Database         ( attachTags, insertImage, selectHashExists
                            , selectImagesCount, selectImage, selectImages
                            , selectNextImage, selectPreviousImage )
import App.FileType         ( ImageFile, File(..) )
import App.Paths            ( imagePath, imageThumbnailPath )
import App.Validation       ( Property(..), Validation, isFalse, isPositive, isValid )
import Control.Monad        ( when )
import Control.Monad.Trans  ( liftIO )
import Control.Monad.Reader ( asks )
import Data.Functor         ( (<$>) )
import Data.Monoid          ( (<>), mconcat )
import Data.Time            ( getCurrentTime )
import Database.Engine      ( Entity, ID )
import Graphics.Thumbnail   ( createThumbnail )
import System.Directory     ( copyFile, createDirectoryIfMissing )
import System.FilePath      ( takeDirectory )
import System.IO.Metadata   ( getHash, getDimensions, getSize )

------------------------------------------------------------------------- Types

-- | A tuple of three values of the same type.
type Triple a = (a, a, a)

-------------------------------------------------------------------------- CRUD

-- | Returns the total number of images satisfying the given expression.
count :: Expression -> App Int
count expression = runDB (selectImagesCount expression)

-- | Returns a page of images based on the given page number and filter.
query :: Expression -> Int -> App [Entity Image]
query expression page = do
    size <- asks configPageSize
    runDB $ selectImages expression ((page - 1) * size) size

-- | Returns the image with the given ID along with the two adjacent images
-- | based on the given filter.
queryTriple :: Expression -> ID -> App (Triple (Maybe (Entity Image)))
queryTriple expression id = runDB $ do
    main <- selectImage id
    next <- selectNextImage id expression
    prev <- selectPreviousImage id expression

    return (prev, main, next)

-- | Inserts a new image into the database/filesystem based on the given file,
-- | title and tags. Returns valid if the insertion was sucessful; otherwise 
-- | invalid.
insert :: ImageFile -> String -> [String] -> App Validation
insert file title tagNames = do
    let fromPath = getPath file
    
    hash        <- liftIO $ getHash fromPath
    now         <- liftIO $ getCurrentTime
    size        <- liftIO $ fromIntegral <$> getSize fromPath
    (w, h)      <- liftIO $ getDimensions fromPath
    isDuplicate <- runDB  $ selectHashExists hash
    thumbSize   <- asks   $ configThumbnailSize

    let ext     = getExtension file
        tags    = Tag.cleanTags tagNames
        image   = Image title False hash ext w h now now size tags
        results = validate image <> isFalse (Property "duplicate" isDuplicate)
                 
    when (isValid results) $ do
        toPath    <- imagePath image
        thumbPath <- imageThumbnailPath image
        
        runDB  $ insertImage image >>= attachTags tags
        liftIO $ createDirectoryIfMissing True $ takeDirectory toPath
        liftIO $ copyFile fromPath toPath
        liftIO $ createThumbnail thumbSize toPath thumbPath
    
    return results

----------------------------------------------------------------------- Utility

-- | Returns valid if all fields of the given image are valid; otherwise 
-- | invalid. Any validation that requires access to the database is ignored.
validate :: Image -> Validation
validate Image {..} = mconcat
    [ isPositive (Property "size" imageFileSize)
    , mconcat    (Tag.validate . Tag <$> imageTagNames) ]
