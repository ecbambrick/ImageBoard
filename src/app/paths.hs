{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module App.Paths where

import App.Common           ( Album(..), Image(..), Page(..) )
import App.Config           ( Config(..) )
import Control.Monad.Reader ( MonadReader, asks )
import Data.Textual         ( replace )
import Database.Engine      ( Entity(..), ID )
import System.FilePath      ( (</>), (<.>) )

------------------------------------------------------------------- Application

-- | Returns the base path of static data.
getDataPath :: String
getDataPath = "data"

-- | Returns the path of the template with the given name.
getTemplatePath :: String -> FilePath
getTemplatePath name = "templates" </> name <.> "html"

------------------------------------------------------------------------ Images

-- | Returns the absolute file path of the given image.
getImagePath :: (MonadReader Config m) => Image -> m FilePath
getImagePath image = do
    storagePath <- asks configStoragePath
    return (storagePath </> getRelativeImagePath image)

-- | Returns the absolute file path of the thumbnail for the given image.
getImageThumbnailPath :: (MonadReader Config m) => Image -> m FilePath
getImageThumbnailPath image = do
    storagePath <- asks configStoragePath
    return (storagePath </> getRelativeImageThumbnailPath image)

-- | Returns the relative URL of the given image
getImageURL :: Image -> FilePath
getImageURL image = toURL (getRelativeImagePath image)

-- | Returns the relative URL of the thumbnail for the given image.
getImageThumbnailURL :: Image -> FilePath
getImageThumbnailURL image = toURL (getRelativeImageThumbnailPath image)

-- | Returns the relative file path of the thumbnail for the given image.
getRelativeImageThumbnailPath :: Image -> FilePath
getRelativeImageThumbnailPath image = "data/thumb" </> take 2 hash </> hash <.> "jpg"
    where hash = imageHash image

-- | Returns the relative file path of the given image.
getRelativeImagePath :: Image -> FilePath
getRelativeImagePath image = "data/image" </> take 2 hash </> hash <.> ext
    where hash = imageHash image
          ext  = imageExtension image

------------------------------------------------------------------------ Albums

-- | Returns the absolute directory path of the given album.
getAlbumPath :: (MonadReader Config m) => ID -> m FilePath
getAlbumPath id = do
    storagePath <- asks configStoragePath
    return (storagePath </> getRelativeAlbumPath id)

-- | Returns the absolute file path of the thumbnail for the given album.
getAlbumThumbnailPath :: (MonadReader Config m) => ID -> m FilePath
getAlbumThumbnailPath id = do
    storagePath <- asks configStoragePath
    return (storagePath </> getRelativeAlbumPath id </> "thumbnail.jpg")

-- | Returns the relative URL of the given album.
getAlbumURL :: Entity Album -> FilePath
getAlbumURL (Entity id _) = toURL (getRelativeAlbumPath id)

-- | Returns the relative URL of the thumbnail of the given album.
getAlbumThumbnailURL :: Entity Album -> FilePath
getAlbumThumbnailURL (Entity id _) = toURL (getRelativeAlbumPath id </> "thumbnail.jpg")

-- | Returns the relative file path of the given album.
getRelativeAlbumPath :: ID -> FilePath
getRelativeAlbumPath id = "data/album" </> show (id `mod` 100) </> show id

------------------------------------------------------------------------- Pages

-- | Returns the absolute directory path of the given page for the album with
-- | the given ID.
getPagePath :: (MonadReader Config m) => ID -> Page -> m FilePath
getPagePath id Page {..} = do
    storagePath <- asks configStoragePath
    basePath    <- getAlbumPath id
    return (basePath </> show pageNumber <.> pageExtension)

-- | Returns the absolute directory path of the thumbnail for the given page 
-- | for the album with the given ID.
getPageThumbnailPath :: (MonadReader Config m) => ID -> Page -> m FilePath
getPageThumbnailPath id Page {..} = do
    storagePath <- asks configStoragePath
    basePath    <- getAlbumPath id
    return (basePath </> "t" ++ show pageNumber <.> "jpg")

-- | Returns the relative URL of the given page.
getPageURL :: ID -> Page -> FilePath
getPageURL id Page {..} = toURL path
    where path = getRelativeAlbumPath id </> show pageNumber <.> pageExtension

-- | Returns the relative URL of the thumbnail of the given page.
getPageThumbnailURL :: ID -> Page -> FilePath
getPageThumbnailURL id Page {..} = toURL path
    where path = getRelativeAlbumPath id </> "t" ++ show pageNumber <.> "jpg"

----------------------------------------------------------------------- Utility

-- | Converts the given file path to a URL.
toURL :: FilePath -> FilePath
toURL path = replace "\\" "/" ("/" ++ path)
