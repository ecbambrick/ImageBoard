{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Storage.FileSystem where

import qualified App.Config           as Config
import qualified App.Storage.Path     as Path
import qualified Data.ByteString.Lazy as ByteString
import qualified Graphics.FFmpeg      as Graphics
import qualified System.Directory     as Directory
import qualified System.FilePath      as FilePath

import App.Core.Types       ( Image(..), Page(..), App, ID )
import Control.Monad        ( forM_, when )
import Control.Monad.Trans  ( liftIO )
import Data.ByteString.Lazy ( ByteString )
import System.IO            ( IOMode(..) )

------------------------------------------------------------------------ Albums

-- | Deletes all files related to the album with the given ID.
deleteAlbum :: ID -> App ()
deleteAlbum albumID = do
    path <- Path.albumDirectory albumID

    liftIO $! do
        pathExists <- Directory.doesDirectoryExist path
        when pathExists (Directory.removeDirectoryRecursive path)

-- | Saves a new album to the file system based on the given ID and page data.
addAlbum :: ID -> [(Page, ByteString)] -> App ()
addAlbum albumID pages = do
    directory      <- Path.albumDirectory albumID
    firstPagePath  <- Path.pageFile albumID (fst $ head pages)
    albumthumbPath <- Path.albumThumb albumID
    thumbSize      <- Config.thumbnailSize

    liftIO $ Directory.createDirectoryIfMissing True directory

    forM_ pages $ \(page, content) -> do
        pagePath      <- Path.pageFile  albumID page
        pageThumbPath <- Path.pageThumb albumID page

        liftIO $! ByteString.writeFile pagePath content
        Graphics.createThumbnail thumbSize pagePath pageThumbPath

    Graphics.createThumbnail thumbSize firstPagePath albumthumbPath

------------------------------------------------------------------------ Images

-- | Deletes all files related to the given image.
deleteImage :: Image -> App ()
deleteImage image = do
    imagePath <- Path.imageFile image
    thumbPath <- Path.imageThumb image

    liftIO $! do
        imageExists <- Directory.doesFileExist imagePath
        thumbExists <- Directory.doesFileExist thumbPath
        when imageExists (Directory.removeFile imagePath)
        when thumbExists (Directory.removeFile thumbPath)

-- | Saves a new image to the file system based on the given image and path.
addImage :: Image -> FilePath -> App ()
addImage image fromPath = do
    toPath    <- Path.imageFile  image
    thumbPath <- Path.imageThumb image
    thumbSize <- Config.thumbnailSize

    liftIO $! do
        Directory.createDirectoryIfMissing True (FilePath.takeDirectory toPath)
        Directory.copyFile fromPath toPath
        Graphics.createThumbnail thumbSize toPath thumbPath
