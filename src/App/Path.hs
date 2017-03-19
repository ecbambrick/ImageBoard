{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module App.Path where

import App.Config           ( Config(..) )
import App.Core.Types       ( Album(..), Image(..), Page(..), ID )
import Control.Monad.Reader ( MonadIO, MonadReader, asks, liftIO, unless )
import Data.Textual         ( replace )
import System.Directory     ( doesDirectoryExist )
import System.FilePath      ( (</>), (<.>), isValid )

------------------------------------------------------------------- Application

-- | Returns the path prefix for data files.
dataPrefix :: String
dataPrefix = "data"

-- | Returns the path prefix for static files.
staticPrefix :: String
staticPrefix = "static"

-- | Returns the path prefix for API methods.
apiPrefix :: String
apiPrefix = "api"

-- | Returns the path to the data directory.
dataDirectory :: (MonadReader Config m) => m FilePath
dataDirectory = do
    storagePath <- asks configStoragePath
    return (storagePath </> dataPrefix)

------------------------------------------------------------------------ Images

-- | Returns the absolute file path of the given image.
imageFile :: (MonadReader Config m) => Image -> m FilePath
imageFile image = do
    storagePath <- asks configStoragePath
    return (storagePath </> relativeImageFile image)

-- | Returns the absolute file path for the thumbnail of the given image.
imageThumb :: (MonadReader Config m) => Image -> m FilePath
imageThumb image = do
    storagePath <- asks configStoragePath
    return (storagePath </> relativeImageThumb image)

-- | Returns the relative file path for the thumbnail of the given image.
relativeImageThumb :: Image -> FilePath
relativeImageThumb image = base </> take 2 hash </> hash <.> "jpg"
    where base = dataPrefix </> "thumb"
          hash = imageHash image

-- | Returns the relative file path of the given image.
relativeImageFile :: Image -> FilePath
relativeImageFile image = base </> take 2 hash </> hash <.> ext
    where base = dataPrefix </> "image"
          hash = imageHash image
          ext  = imageExtension image

------------------------------------------------------------------------ Albums

-- | Returns the absolute directory path of the album with the given ID.
albumDirectory :: (MonadReader Config m) => ID -> m FilePath
albumDirectory id = do
    storagePath <- asks configStoragePath
    return (storagePath </> relativeAlbumDirectory id)

-- | Returns the absolute file path for the thumbnail of the album with the
-- | given ID.
albumThumb :: (MonadReader Config m) => ID -> m FilePath
albumThumb id = do
    storagePath <- asks configStoragePath
    return (storagePath </> relativeAlbumDirectory id </> "thumbnail.jpg")

-- | Returns the relative directory path of the album with the given ID.
relativeAlbumDirectory :: ID -> FilePath
relativeAlbumDirectory id = base </> show (id `mod` 100) </> show id
    where base = dataPrefix </> "album"

------------------------------------------------------------------------- Pages

-- | Returns the absolute file path for the given page of the album with
-- | the given ID.
pageFile :: (MonadReader Config m) => ID -> Page -> m FilePath
pageFile id Page {..} = do
    storagePath <- asks configStoragePath
    basePath    <- albumDirectory id
    return (basePath </> show pageNumber <.> pageExtension)

-- | Returns the absolute file path for the thumbnail for the given page
-- | of the album with the given ID.
pageThumb :: (MonadReader Config m) => ID -> Page -> m FilePath
pageThumb id Page {..} = do
    storagePath <- asks configStoragePath
    basePath    <- albumDirectory id
    return (basePath </> "t" ++ show pageNumber <.> "jpg")
