{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module App.Storage.Path where

import qualified App.Config as Config

import App.Config           ( Config )
import App.Core.Types       ( Album(..), Image(..), Page(..), ID )
import Control.Monad.Reader ( MonadReader )
import System.FilePath      ( (</>), (<.>) )

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

-- | Returns the relative file path of the given image.
relativeImageFile :: Image -> FilePath
relativeImageFile image = "images" </> take 2 hash </> hash <.> ext
    where hash = imageHash image
          ext  = imageExtension image

-- | Returns the relative file path for the thumbnail of the given image.
relativeImageThumb :: Image -> FilePath
relativeImageThumb image = "thumbs-images" </> take 2 hash </> hash <.> "jpg"
    where hash = imageHash image

------------------------------------------------------------------------ Albums

-- | Returns the absolute directory path of the album with the given ID.
albumDirectory :: (MonadReader Config m) => ID -> m FilePath
albumDirectory id = do
    baseDirectory <- dataDirectory
    return (baseDirectory </> relativeAlbumDirectory id)

-- | Returns the absolute directory path of the thumbnails for the album with
-- | the given ID.
albumThumbDirectory :: (MonadReader Config m) => ID -> m FilePath
albumThumbDirectory id = do
    baseDirectory <- dataDirectory
    return (baseDirectory </> relativeAlbumThumbDirectory id)

-- | Returns the absolute file path for the thumbnail of the album with the
-- | given ID.
albumThumb :: (MonadReader Config m) => ID -> m FilePath
albumThumb id = do
    baseDirectory <- dataDirectory
    return (baseDirectory </> relativeAlbumThumb id)

-- | Returns the relative directory path of the album with the given ID.
relativeAlbumDirectory :: ID -> FilePath
relativeAlbumDirectory id = "albums" </> prefix </> show id
    where prefix = show (id `mod` 100)

-- | Returns the relative file directory path for the thumbnails of the given
-- | album.
relativeAlbumThumbDirectory :: ID -> FilePath
relativeAlbumThumbDirectory id = "thumbs-albums" </> prefix </> show id
    where prefix = show (id `mod` 100)

-- | Returns the relative file path for the thumbnail of the album with the
-- | given ID.
relativeAlbumThumb :: ID -> FilePath
relativeAlbumThumb id = relativeAlbumThumbDirectory id </> "thumb.jpg"

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
    baseDirectory <- albumThumbDirectory id
    return (baseDirectory </> show pageNumber <.> "jpg")

-- | Returns the relative file path for the given page of the album with the 
-- | given ID.
relativePageFile :: ID -> Page -> FilePath
relativePageFile id Page {..} = directory </> page <.> ext
    where directory = relativeAlbumDirectory id
          page      = show pageNumber
          ext       = pageExtension

-- | Returns the relative file path for the thumbnail of the given page of the
-- | album with the given ID.
relativePageThumb :: ID -> Page -> FilePath
relativePageThumb id Page {..} = directory </> page <.> "jpg"
    where directory = relativeAlbumThumbDirectory id
          page      = show pageNumber
