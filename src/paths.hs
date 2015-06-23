{-# LANGUAGE FlexibleContexts #-}

module Paths where

import Common               ( Config(..), Image(..) )
import Control.Monad.Reader ( MonadReader, asks )
import Data.Textual         ( replace )
import System.FilePath      ( (</>), (<.>) )

-- | Returns the absolute file path of the given image.
absoluteImagePath :: (MonadReader Config m) => Image -> m FilePath
absoluteImagePath image = do
    storagePath <- asks configStoragePath
    return $ storagePath </> relativeImagePath image

-- | Returns the absolute path of the storage directory for images.
absoluteImagesDir :: (MonadReader Config m) => m FilePath
absoluteImagesDir = do
    storagePath <- asks configStoragePath
    return $ storagePath </> relativeImagesDir
    
-- | Returns the relative file path of the given image
relativeImagePath :: Image -> FilePath
relativeImagePath image = relativeImagesDir </> take 2 hash </> hash <.> ext
    where hash = imageHash image
          ext  = imageExtension image

-- | Returns the relative path of the storage directory for images.
relativeImagesDir :: FilePath
relativeImagesDir = "images"

-- | Returns the relative url of the given image
relativeImageURL :: Image -> FilePath
relativeImageURL image = replace "\\" "/" $ relativeImagePath image
