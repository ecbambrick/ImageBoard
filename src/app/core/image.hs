module App.Core.Image 
    ( getAll, getFiltered, getSingle, getNext, getPrevious, insert ) where

import qualified App.Core.Tag as Tag

import App.Common               ( Tag(..), Image(..), App, Entity, ID, runDB )
import App.Config               ( Config(..) )
import App.Expression           ( Expression, Token(..) )
import App.DataSource.SQLite    ( attachTags, insertImage, selectHashExists
                                , selectImage, selectImages
                                , selectImagesByExpression, selectNextImage
                                , selectPreviousImage )
import App.Paths                ( absoluteImagePath, absoluteThumbPath )
import App.Validation           ( Property(..), Validation(..), isFalse
                                , isPositive, isValidImageFileType, isValid )
import Control.Concurrent.Async ( async, wait )
import Control.Monad            ( when )
import Control.Monad.Trans      ( liftIO )
import Control.Monad.Reader     ( asks )
import Data.Functor             ( (<$>) )
import Data.List                ( nub )
import Data.Monoid              ( (<>), mconcat )
import Data.Textual             ( strip, toLower )
import Data.Time                ( getCurrentTime )
import System.Directory         ( copyFile, createDirectoryIfMissing )
import System.FilePath          ( takeBaseName, takeDirectory, takeExtension
                                , replaceExtension )
import System.IO.Metadata       ( getHash, getDimensions, getSize )
import System.Process           ( runCommand, waitForProcess )
import Text.Printf              ( printf )

-- | Returns the list of all image entities.
getAll :: App [Entity Image]
getAll = runDB $ selectImages

-- | Returns the list of image entities that satisfy the given filter.
getFiltered :: Expression -> App [Entity Image]
getFiltered = runDB . selectImagesByExpression

-- | Returns the image with the given ID or nothing if not found.
getSingle :: ID -> App (Maybe (Entity Image))
getSingle = runDB . selectImage

-- | Returns the image following the image with the given ID or nothing if no
-- | images exist.
getNext :: ID -> App (Maybe (Entity Image))
getNext = runDB . selectNextImage

-- | Returns the image preceeding the image with the given ID or nothing if no
-- | images exist.
getPrevious :: ID -> App (Maybe (Entity Image))
getPrevious = runDB . selectPreviousImage

-- | Inserts a new image into the database/filesystem based on the the file 
-- | with the given path and the given tags. Returns valid if the insertion was
-- | sucessful; otherwise invalid.
insert :: FilePath -> String -> [String] -> App Validation
insert fromPath fileName tagNames = case fileType of
    Valid     -> insert'
    Invalid e -> return (Invalid e)
    where
        fileType = isValidImageFileType (Property "extension" ext)
        ext      = getExtension fileName
        insert'  = do
            hash        <- liftIO $ getHash fromPath
            now         <- liftIO $ getCurrentTime
            size        <- liftIO $ fromIntegral <$> getSize fromPath
            (w, h)      <- liftIO $ getDimensions fromPath
            isDuplicate <- runDB  $ selectHashExists hash
            storagePath <- asks   $ configStoragePath

            let title   = takeBaseName fileName
                tags    = cleanTags tagNames
                image   = Image title False hash ext w h now now size []
                results = validate image
                          <> isFalse (Property "duplicate" isDuplicate)
                          <> mconcat (Tag.validate . Tag <$> tags)
                         
            when (isValid results) $ do
                toPath    <- absoluteImagePath image
                thumbPath <- absoluteThumbPath image
                runDB  $ insertImage image >>= attachTags tags
                liftIO $ createDirectoryIfMissing True $ takeDirectory toPath
                liftIO $ copyFile fromPath toPath
                liftIO $ thumbnail 256 toPath thumbPath
            
            return results

-- | Returns valid if all fields of the given image are valid; otherwise 
-- | invalid. Any validation that requires access to the database is ignored.
validate :: Image -> Validation
validate Image { imageFileSize = size } = isPositive (Property "size" size)

----------------------------------------------------------------------- Utility

-- | Returns the file extension of the given path, excluding the period.
getExtension :: FilePath -> String
getExtension path = if null extension then "" else toLower (tail extension)
    where extension = takeExtension path

-- | Sanitizes the given list of tag names.
cleanTags :: [String] -> [String]
cleanTags = filter (not . null) . nub . map (toLower . strip)

-- | Generates a thumbnail of the given size from the first given file path to 
-- | the second given file path.
thumbnail :: Int -> FilePath -> FilePath -> IO ()
thumbnail size from to = do
    createDirectoryIfMissing True $ takeDirectory to
    runCommand cmd >>= waitForProcess
    return ()
    where 
        cmd  = printf text (size*2) (size*2) size size size size
        text = unwords 
            [ "convert"
            , "-define jpeg:size=%dx%d"
            , "-background white"
            , "-format jpg"
            , "-thumbnail \"%dx%d^\""
            , "-gravity center"
            , "-extent %dx%d"
            , if takeExtension from == ".gif" then from ++ "[0]" else from
            , replaceExtension to "jpg" ]
