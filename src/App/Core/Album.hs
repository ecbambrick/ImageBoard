{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Album
    ( count, delete, getPage, insert, query, querySingle, update ) where

import qualified App.Config           as Config
import qualified App.Core.Tag         as Tag
import qualified App.Storage.Database as DB
import qualified App.Storage.Path     as Path
import qualified App.Validation       as Validation
import qualified Data.ByteString      as ByteString
import qualified Data.DateTime        as DateTime
import qualified Graphics.FFmpeg      as Graphics
import qualified System.IO.Metadata   as Metadata

import App.Control          ( runDB )
import App.Core.Types       ( Album(..), DeletionMode(..), Page(..), App, ID )
import App.Expression       ( Expression )
import App.Validation       ( Error(..), Validation )
import Codec.Archive.Zip    ( Archive(..), Entry(..), toArchive, fromEntry )
import Control.Monad        ( unless, when )
import Control.Monad.Trans  ( liftIO )
import Data.ByteString.Lazy ( hGetContents, hPut )
import Data.List            ( (\\), sortBy, find )
import Data.Ord.Extended    ( comparingAlphaNum )
import Data.Textual         ( trim )
import System.FilePath      ( dropExtension, takeExtension )
import System.Directory     ( createDirectoryIfMissing, doesDirectoryExist
                            , removeDirectoryRecursive )
import System.IO            ( IOMode(..), hClose, openFile, withFile )

-------------------------------------------------------------------------- CRUD

-- | Returns the total number of albums satisfying the given expression.
count :: Expression -> App Int
count = runDB . DB.selectAlbumsCount

-- | Deletes the album with the given ID. If the delete is permanent, it is
-- | removed from the database/file system; otherwise, it is just marked as
-- | deleted.
delete :: DeletionMode -> ID -> App ()
delete MarkAsDeleted id = runDB (DB.markPostAsDeleted id)
delete PermanentlyDelete id = do
    path <- Path.albumDirectory id

    liftIO $ do
        pathExists <- doesDirectoryExist path
        when pathExists (removeDirectoryRecursive path)

    runDB $ do
        DB.deletePost id
        DB.cleanTags

-- | Returns the page with the given number from the given album.
getPage :: Album -> Int -> Maybe Page
getPage Album {..} number = find (\x -> number == pageNumber x) albumPages

-- | Inserts a new album into the database/filesystem based on the given file
-- | path, title and tags. Returns valid if the insertion was sucessful;
-- | otherwise, invalid.
insert :: FilePath -> String -> [String] -> App Validation
insert path title tagNames = do
    file    <- liftIO $ openFile path ReadMode
    archive <- liftIO $ toArchive <$> hGetContents file
    now     <- DateTime.now

    let entries    = getImages archive
        entryPairs = zip entries [1..]
        pages      = map (uncurry toPage) entryPairs
        fileSize   = sum $ map (fromIntegral . eUncompressedSize) entries
        tags       = Tag.cleanTags tagNames
        album      = Album 0 (trim title) False now now fileSize pages tags
        results    = validate album

    when (Validation.isValid results) $ do
        id <- runDB $ do
            id <- DB.insertAlbum album
            DB.attachTags tags id
            return id

        unless (null entries) $ do
            basePath  <- Path.albumDirectory id
            firstPath <- Path.pageFile id (toPage (head entries) 1)
            thumbPath <- Path.albumThumb id
            thumbSize <- Config.thumbnailSize

            liftIO $ createDirectoryIfMissing True basePath
            mapM_ (extractFile id) entryPairs
            liftIO $ Graphics.createThumbnail thumbSize firstPath thumbPath

    liftIO $ hClose file

    return results

-- | Returns a page of albums based on the given page number and filter.
query :: Expression -> Int -> App [Album]
query expression page = do
    size <- Config.pageSize
    runDB $ DB.selectAlbums expression ((page - 1) * size) size

-- | Returns the album with the given ID.
querySingle :: ID -> App (Maybe Album)
querySingle = runDB . DB.selectAlbum

-- | Updates the given album in the database. Returns valid if the update was
-- | successful; otherwise, invalid.
update :: ID -> String -> [String] -> App Validation
update id title tags = do
    now      <- DateTime.now
    previous <- runDB $ DB.selectAlbum id

    case previous of
        Just previousAlbum -> do
            let result   = validate newAlbum
                newAlbum = previousAlbum
                    { albumTags = Tag.cleanTags tags
                    , albumTitle    = trim title
                    , albumModified = now }

            when (Validation.isValid result) $
                runDB $ do
                    let newTags = albumTags newAlbum
                        oldTags = albumTags previousAlbum

                    DB.updateAlbum newAlbum
                    DB.detachTags (oldTags \\ newTags) id
                    DB.attachTags (newTags \\ oldTags) id
                    DB.cleanTags

            return result

        Nothing -> do
            return (Validation.invalidate (IDNotFound id))

----------------------------------------------------------------------- Utility

-- | Returns valid if all fields of the given album are valid; otherwise
-- | invalid.
validate :: Album -> Validation
validate Album {..} = Tag.validateMany albumTags

-- | Extracts the given zip file entry.
extractFile :: ID -> (Entry, Int) -> App ()
extractFile id (entry, index) = do
    let contents = fromEntry entry
        page     = toPage entry index

    extractPath   <- Path.pageFile id page
    thumbnailPath <- Path.pageThumb id page
    thumbSize     <- Config.thumbnailSize

    liftIO $ do
        withFile extractPath WriteMode (flip hPut contents)
        Graphics.createThumbnail thumbSize extractPath thumbnailPath

-- | Creates a new page from the given zip file entry and index.
toPage :: Entry -> Int -> Page
toPage Entry {..} index = Page index title ext
    where title = dropExtension eRelativePath
          ext   = drop 1 (takeExtension eRelativePath)

-- | Gets the list of image entries from the given archive.
getImages :: Archive -> [Entry]
getImages archive =
    let entries = sortBy (comparingAlphaNum eRelativePath) (zEntries archive)
    in flip filter entries $ \entry ->
        let mimeType = Metadata.getMIMETypeFromBytes (fromEntry entry)
        in case mimeType of
            Just ("image",      _) -> True
            Just ("video", "webm") -> True
            _                      -> False
