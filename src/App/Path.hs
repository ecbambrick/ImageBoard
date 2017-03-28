{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module App.Path where

import qualified App.Config as Config

import App.Config           ( Config )
import App.Core.Types       ( Album(..), Image(..), Page(..), ID )
import Control.Monad.Reader ( MonadReader, liftIO, unless )
import Data.Textual         ( replace )
import System.Directory     ( doesDirectoryExist )
import System.FilePath      ( (</>), (<.>), isValid )

------------------------------------------------------------------- Application

-- | Returns the path to the data directory.
dataDirectory :: (MonadReader Config m) => m FilePath
dataDirectory = Config.storagePath

------------------------------------------------------------------------ Images

-- | Returns the absolute file path of the given image.
imageFile :: (MonadReader Config m) => Image -> m FilePath
imageFile image = do
    baseDirectory <- dataDirectory
    return (baseDirectory </> relativeImageFile image)

-- | Returns the absolute file path for the thumbnail of the given image.
imageThumb :: (MonadReader Config m) => Image -> m FilePath
imageThumb image = do
    baseDirectory <- dataDirectory
    return (baseDirectory </> relativeImageThumb image)

-- | Returns the relative file path for the thumbnail of the given image.
relativeImageThumb :: Image -> FilePath
relativeImageThumb image = "thumb" </> take 2 hash </> hash <.> "jpg"
    where hash = imageHash image

-- | Returns the relative file path of the given image.
relativeImageFile :: Image -> FilePath
relativeImageFile image = "image" </> take 2 hash </> hash <.> ext
    where hash = imageHash image
          ext  = imageExtension image

------------------------------------------------------------------------ Albums

-- | Returns the absolute directory path of the album with the given ID.
albumDirectory :: (MonadReader Config m) => ID -> m FilePath
albumDirectory id = do
    baseDirectory <- dataDirectory
    return (baseDirectory </> relativeAlbumDirectory id)

-- | Returns the absolute file path for the thumbnail of the album with the
-- | given ID.
albumThumb :: (MonadReader Config m) => ID -> m FilePath
albumThumb id = do
    baseDirectory <- dataDirectory
    return (baseDirectory </> relativeAlbumDirectory id </> "thumbnail.jpg")

-- | Returns the relative directory path of the album with the given ID.
relativeAlbumDirectory :: ID -> FilePath
relativeAlbumDirectory id = "album" </> show (id `mod` 100) </> show id

------------------------------------------------------------------------- Pages

-- | Returns the absolute file path for the given page of the album with
-- | the given ID.
pageFile :: (MonadReader Config m) => ID -> Page -> m FilePath
pageFile id Page {..} = do
    baseDirectory <- albumDirectory id
    return (baseDirectory </> show pageNumber <.> pageExtension)

-- | Returns the absolute file path for the thumbnail for the given page
-- | of the album with the given ID.
pageThumb :: (MonadReader Config m) => ID -> Page -> m FilePath
pageThumb id Page {..} = do
    baseDirectory <- albumDirectory id
    return (baseDirectory </> "t" ++ show pageNumber <.> "jpg")
