{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module App.Paths 
    ( dataPath, templatePath, imagePath, imageThumbnailPath, imageURL
    , imageThumbnailURL, albumPath, albumThumbnailPath, albumURL
    , albumThumbnailURL, pagePath, pageThumbnailPath, pageURL
    , pageThumbnailURL ) where

import App.Common           ( Album(..), Image(..), Page(..) )
import App.Config           ( Config(..) )
import Control.Monad.Reader ( MonadReader, asks )
import Data.Textual         ( replace )
import Database.Engine      ( Entity(..), ID )
import System.FilePath      ( (</>), (<.>) )

------------------------------------------------------------------- Application

-- | Returns the base path of static data.
dataPath :: String
dataPath = "data"

-- | Returns the path of the template with the given name.
templatePath :: String -> FilePath
templatePath name = "templates" </> name <.> "html"

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
    return (storagePath </> relativeImageThumbnailPath image)

-- | Returns the relative URL of the given image
imageURL :: Image -> FilePath
imageURL image = toURL (relativeImagePath image)

-- | Returns the relative URL of the thumbnail for the given image.
imageThumbnailURL :: Image -> FilePath
imageThumbnailURL image = toURL (relativeImageThumbnailPath image)

-- | Returns the relative file path of the thumbnail for the given image.
relativeImageThumbnailPath :: Image -> FilePath
relativeImageThumbnailPath image = "data/thumb" </> take 2 hash </> hash <.> "jpg"
    where hash = imageHash image

-- | Returns the relative file path of the given image.
relativeImagePath :: Image -> FilePath
relativeImagePath image = "data/image" </> take 2 hash </> hash <.> ext
    where hash = imageHash image
          ext  = imageExtension image

------------------------------------------------------------------------ Albums

-- | Returns the absolute directory path of the given album.
albumPath :: (MonadReader Config m) => ID -> m FilePath
albumPath id = do
    storagePath <- asks configStoragePath
    return (storagePath </> relativeAlbumPath id)

-- | Returns the absolute file path of the thumbnail for the given album.
albumThumbnailPath :: (MonadReader Config m) => ID -> m FilePath
albumThumbnailPath id = do
    storagePath <- asks configStoragePath
    return (storagePath </> relativeAlbumPath id </> "thumbnail.jpg")

-- | Returns the relative URL of the given album.
albumURL :: Entity Album -> FilePath
albumURL (Entity id _) = toURL (relativeAlbumPath id)

-- | Returns the relative URL of the thumbnail of the given album.
albumThumbnailURL :: Entity Album -> FilePath
albumThumbnailURL (Entity id _) = toURL (relativeAlbumPath id </> "thumbnail.jpg")

-- | Returns the relative file path of the given album.
relativeAlbumPath :: ID -> FilePath
relativeAlbumPath id = "data/album" </> show (id `mod` 100) </> show id

------------------------------------------------------------------------- Pages

-- | Returns the absolute directory path of the given page for the album with
-- | the given ID.
pagePath :: (MonadReader Config m) => ID -> Page -> m FilePath
pagePath id Page {..} = do
    storagePath <- asks configStoragePath
    basePath    <- albumPath id
    return (basePath </> show pageNumber <.> pageExtension)

-- | Returns the absolute directory path of the thumbnail for the given page 
-- | for the album with the given ID.
pageThumbnailPath :: (MonadReader Config m) => ID -> Page -> m FilePath
pageThumbnailPath id Page {..} = do
    storagePath <- asks configStoragePath
    basePath    <- albumPath id
    return (basePath </> "t" ++ show pageNumber <.> "jpg")

-- | Returns the relative URL of the given page.
pageURL :: ID -> Page -> FilePath
pageURL id Page {..} = toURL path
    where path = relativeAlbumPath id </> show pageNumber <.> pageExtension

-- | Returns the relative URL of the thumbnail of the given page.
pageThumbnailURL :: ID -> Page -> FilePath
pageThumbnailURL id Page {..} = toURL path
    where path = relativeAlbumPath id </> "t" ++ show pageNumber <.> "jpg"

----------------------------------------------------------------------- Utility

-- | Converts the given file path to a URL.
toURL :: FilePath -> FilePath
toURL path = replace "\\" "/" ("/" ++ path)
