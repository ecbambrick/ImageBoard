module App.Core.Image ( get, insert ) where

import qualified App.Core.Tag as Tag

import App.Common            ( Config(..), Tag(..), Image(..), Entity, App
                             , runDB )
import App.DataSource.SQLite ( attachTags, insertImage, selectHashExists
                             , selectImages )
import App.Paths             ( absoluteImagePath )
import App.Validation        ( Property(..), Validation(..), isFalse, isPositive
                             , isValidImageFileType, isValid )
import Control.Monad         ( when )
import Control.Monad.Trans   ( liftIO )
import Control.Monad.Reader  ( asks )
import Data.Functor          ( (<$>) )
import Data.Monoid           ( (<>), mconcat )
import Data.Textual          ( strip, toLower )
import Data.Time             ( getCurrentTime )
import System.Directory      ( copyFile, createDirectoryIfMissing )
import System.FilePath       ( takeBaseName, takeDirectory, takeExtension )
import System.IO.Metadata    ( getHash, getDimensions, getSize )

-- | Returns the list of all image entities.
get :: App [Entity Image]
get = runDB selectImages

-- | Inserts a new image into the database/filesystem based on the the file 
-- | with the given path and the given tags. Returns valid if the insertion was
-- | sucessful; otherwise invalid.
insert :: FilePath -> [String] -> App Validation
insert fromPath tagNames = case fileType of
    Valid     -> insert'
    Invalid e -> return (Invalid e)
    where
        fileType = isValidImageFileType (Property "extension" ext)
        ext      = getExtension fromPath
        insert'  = do
            hash        <- liftIO $ getHash fromPath
            now         <- liftIO $ getCurrentTime
            size        <- liftIO $ fromIntegral <$> getSize fromPath
            (w, h)      <- liftIO $ getDimensions fromPath
            isDuplicate <- runDB  $ selectHashExists hash
            storagePath <- asks   $ configStoragePath

            let title   = takeBaseName fromPath
                tags    = filter (not . null) $ toLower <$> strip <$> tagNames
                image   = Image title False hash ext w h now now size
                results = validate image
                          <> isFalse (Property "duplicate" isDuplicate)
                          <> mconcat (Tag.validate . Tag <$> tags)
                         
            when (isValid results) $ do
                toPath <- absoluteImagePath image
                runDB  $ insertImage image >>= attachTags tags
                liftIO $ createDirectoryIfMissing True $ takeDirectory toPath
                liftIO $ copyFile fromPath toPath
            
            return results

-- | Returns valid if all fields of the given image are valid; otherwise 
-- | invalid. Any validation that requires access to the database is ignored.
validate :: Image -> Validation
validate (Image _ _ _ _ _ _ _ _ size) = isPositive (Property "size" size)

----------------------------------------------------------------------- Utility

-- | Returns the file extension of the given path, excluding the period.
getExtension :: FilePath -> String
getExtension path = if null extension then "" else map toLower (tail extension)
    where extension = takeExtension path
