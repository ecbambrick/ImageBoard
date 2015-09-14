module App.Core.Image ( query, queryTriple, insert ) where

import qualified App.Core.Tag as Tag

import App.Common           ( Tag(..), Image(..), App, runDB )
import App.Config           ( Config(..) )
import App.Expression       ( Expression, Token(..) )
import App.Database         ( attachTags, insertImage, selectHashExists
                            , selectImage, selectImages, selectNextImage
                            , selectPreviousImage )
import App.Paths            ( absoluteImagePath, absoluteThumbPath )
import App.Validation       ( Property(..), Validation(..), isFalse, isPositive
                            , isValidImageFileType, isValid )
import Control.Monad        ( when )
import Control.Monad.Trans  ( liftIO )
import Control.Monad.Reader ( asks )
import Data.Functor         ( (<$>) )
import Data.List            ( nub )
import Data.Monoid          ( (<>), mconcat )
import Data.Textual         ( strip, toLower )
import Data.Time            ( getCurrentTime )
import Database.Engine      ( Entity, ID )
import System.Directory     ( copyFile, createDirectoryIfMissing )
import System.FilePath      ( takeBaseName, takeDirectory, takeExtension
                            , replaceExtension )
import System.IO.Metadata   ( getHash, getDimensions, getSize )
import System.Process       ( runCommand, waitForProcess )
import Text.Printf          ( printf )

------------------------------------------------------------------------- Types

-- | A tuple of three values of the same type.
type Triple a = (a, a, a)

-------------------------------------------------------------------------- CRUD

-- | Returns the list of all image entities.
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

-- | Inserts a new image into the database/filesystem based on the the file 
-- | with the given path and the given title and tags. Returns valid if the 
-- | insertion was sucessful; otherwise invalid.
insert :: FilePath -> String -> String -> [String] -> App Validation
insert fromPath fileName title tagNames = case fileType of
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

            let tags    = cleanTags tagNames
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
