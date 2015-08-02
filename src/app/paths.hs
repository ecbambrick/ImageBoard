{-# LANGUAGE FlexibleContexts #-}

module App.Paths where

import App.Common           ( Image(..) )
import App.Config           ( Config(..) )
import Control.Monad.Reader ( MonadReader, asks )
import Data.Textual         ( replace )
import System.FilePath      ( (</>), (<.>) )

--------------------------------------------------------------------- Templates

-- | Returns the path of the template with the given name.
templatePath :: String -> FilePath
templatePath name = "templates" </> name <.> "html"

------------------------------------------------------------------------ Images

-- | Returns the absolute file path of the given image.
absoluteImagePath :: (MonadReader Config m) => Image -> m FilePath
absoluteImagePath image = do
    storagePath <- asks configStoragePath
    return (storagePath </> relativeImagePath image)

-- | Returns the relative file path of the given image.
relativeImagePath :: Image -> FilePath
relativeImagePath image = "images" </> take 2 hash </> hash <.> ext
    where hash = imageHash image
          ext  = imageExtension image

-- | Returns the relative URL of the given image
imageURL :: Image -> FilePath
imageURL image = replace "\\" "/" ("/" ++ relativeImagePath image)

-------------------------------------------------------------------- Thumbnails

-- | Returns the absolute file path of the thumbnail for the given image.
absoluteThumbPath :: (MonadReader Config m) => Image -> m FilePath
absoluteThumbPath image = do
    storagePath <- asks configStoragePath
    return (storagePath </> relativeThumbPath image)

-- | Returns the relative file path of the thumbnail for the given image.
relativeThumbPath :: Image -> FilePath
relativeThumbPath image = "thumbs" </> take 2 hash </> hash <.> "jpg"
    where hash = imageHash image
          ext  = imageExtension image

-- | Returns the relative URL of the thumbnail for the given image.
thumbURL :: Image -> FilePath
thumbURL image = replace "\\" "/" ("/" ++ relativeThumbPath image)
