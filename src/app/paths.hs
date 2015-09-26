{-# LANGUAGE FlexibleContexts #-}

module App.Paths where

import App.Common           ( Image(..) )
import App.Config           ( Config(..) )
import Control.Monad.Reader ( MonadReader, asks )
import Data.Textual         ( replace )
import Database.Engine      ( Entity(..), ID )
import System.FilePath      ( (</>), (<.>) )

------------------------------------------------------------------- Application

-- | Returns the path of the template with the given name.
templatePath :: String -> FilePath
templatePath name = "templates" </> name <.> "html"

-- | Returns the base path of static data.
dataPath :: String
dataPath = "data"

------------------------------------------------------------------------ Images

-- | Returns the absolute file path of the given image.
imagePath :: (MonadReader Config m) => Image -> m FilePath
imagePath image = do
    storagePath <- asks configStoragePath
    return (storagePath </> relativeImagePath image)

-- | Returns the absolute file path of the thumbnail for the given image.
imageThumbnailPath :: (MonadReader Config m) => Image -> m FilePath
imageThumbnailPath image = do
    storagePath <- asks configStoragePath
    return (storagePath </> relativeThumbnailPath image)

-- | Returns the relative URL of the given image
imageURL :: Image -> FilePath
imageURL image = toURL (relativeImagePath image)

-- | Returns the relative URL of the thumbnail for the given image.
imageThumbnailURL :: Image -> FilePath
imageThumbnailURL image = toURL (relativeThumbnailPath image)

-- | Returns the relative file path of the thumbnail for the given image.
relativeThumbnailPath :: Image -> FilePath
relativeThumbnailPath image = "data/thumb" </> take 2 hash </> hash <.> "jpg"
    where hash = imageHash image

-- | Returns the relative file path of the given image.
relativeImagePath :: Image -> FilePath
relativeImagePath image = "data/image" </> take 2 hash </> hash <.> ext
    where hash = imageHash image
          ext  = imageExtension image

----------------------------------------------------------------------- Utility

-- | Converts the given file path to a URL.
toURL :: FilePath -> FilePath
toURL path = replace "\\" "/" ("/" ++ path)
