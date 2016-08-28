{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Album
    ( count, delete, getPage, insert, query, querySingle, update ) where

import qualified App.Core.Tag    as Tag
import qualified App.Database    as DB
import qualified App.FileType    as FileType
import qualified App.Path        as Path
import qualified App.Validation  as Validation
import qualified Data.ByteString as ByteString
import qualified Graphics.FFmpeg as Graphics

import App.Config           ( Config(..) )
import App.Control          ( runDB )
import App.Core.Types       ( Album(..), DeletionMode(..), Page(..), Tag(..), App )
import App.Expression       ( Expression )
import App.FileType         ( ArchiveFile, File(..) )
import App.Validation       ( Error(..), Validation )
import Codec.Archive.Zip    ( Archive(..), Entry(..), toArchive, fromEntry )
import Control.Applicative  ( (<$>) )
import Control.Monad        ( when )
import Control.Monad.Trans  ( liftIO )
import Control.Monad.Reader ( asks )
import Data.ByteString.Lazy ( hGetContents, hPut )
import Data.DateTime        ( getCurrentTime )
import Data.List            ( (\\), sortBy, find )
import Data.Maybe           ( isJust, fromJust )
import Data.Monoid          ( (<>), mconcat )
import Data.Ord.Extended    ( comparingAlphaNum )
import Data.Textual         ( trim )
import Database.Engine      ( Entity(..), ID, fromEntity )
import System.FilePath      ( dropExtension, takeBaseName, takeExtension )
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
    path <- Path.getAlbumPath id

    liftIO $ do
        pathExists <- doesDirectoryExist path
        when pathExists (removeDirectoryRecursive path)

    runDB $ do
        DB.deletePost id
        DB.cleanTags

-- | Returns the page with the given number from the given album.
getPage :: Album -> Int -> Maybe Page
getPage Album {..} number = find (\x -> number == pageNumber x) albumPages

-- | Inserts a new album into the database/filesystem based on the given file,
-- | title and tags. Returns valid if the insertion was sucessful; otherwise
-- | invalid.
insert :: ArchiveFile -> String -> [String] -> App Validation
insert file title tagNames = do
    file    <- liftIO $ openFile (getPath file) ReadMode
    archive <- liftIO $ toArchive <$> hGetContents file
    now     <- liftIO $ getCurrentTime

    let entries    = sortBy (comparingAlphaNum eRelativePath) (zEntries archive)
        anyEntries = not (null entries)
        entryPairs = zip entries [1..]
        pages      = filter isImage $ map (uncurry toPage) entryPairs
        fileSize   = sum $ map (fromIntegral . eUncompressedSize) entries
        tags       = Tag.cleanTags tagNames
        album      = Album (trim title) False now now fileSize pages tags
        results    = validate album

    when (Validation.isValid results) $ do
        id <- runDB $ do
            id <- DB.insertAlbum album
            DB.attachTags tags id
            return id

        when anyEntries $ do
            basePath  <- Path.getAlbumPath id
            firstPath <- Path.getPagePath id (toPage (head entries) 1)
            thumbPath <- Path.getAlbumThumbnailPath id
            thumbSize <- asks configThumbnailSize

            liftIO $ createDirectoryIfMissing True basePath
            mapM_ (extractFile id) entryPairs
            liftIO $ Graphics.createThumbnail thumbSize firstPath thumbPath

    liftIO $ hClose file

    return results

-- | Returns a page of albums based on the given page number and filter.
query :: Expression -> Int -> App [Entity Album]
query expression page = do
    size <- asks configPageSize
    runDB $ DB.selectAlbums expression ((page - 1) * size) size

-- | Returns the album with the given ID.
querySingle :: ID -> App (Maybe (Entity Album))
querySingle = runDB . DB.selectAlbum

-- | Updates the given album in the database. Returns valid if the update was
-- | successful; otherwise, invalid.
update :: Entity Album -> App Validation
update (Entity id album) = do
    now <- liftIO $ getCurrentTime

    runDB $ do
        previous <- DB.selectAlbum id

        let isFound    = Validation.verify (isJust previous) (IDNotFound id)
            cleanAlbum = album { albumTagNames = Tag.cleanTags (albumTagNames album) }
            results    = validate album <> isFound

        when (Validation.isValid results) $ do
            let newTags = albumTagNames $ album
                oldTags = albumTagNames $ fromEntity $ fromJust previous

            DB.updateAlbum (Entity id album { albumModified = now })
            DB.detachTags (oldTags \\ newTags) id
            DB.attachTags (newTags \\ oldTags) id
            DB.cleanTags

        return results

----------------------------------------------------------------------- Utility

-- | Returns valid if all fields of the given album are valid; otherwise
-- | invalid.
validate :: Album -> Validation
validate Album {..} = Tag.validateMany $ map Tag albumTagNames

-- | Extracts the given zip file entry.
extractFile :: ID -> (Entry, Int) -> App ()
extractFile id (entry, index) = do
    let contents = fromEntry entry
        page     = toPage entry index

    extractPath   <- Path.getPagePath id page
    thumbnailPath <- Path.getPageThumbnailPath id page
    thumbSize     <- asks configThumbnailSize

    liftIO $ do
        withFile extractPath WriteMode (flip hPut contents)
        Graphics.createThumbnail thumbSize extractPath thumbnailPath

-- | Creates a new page from the given zip file entry and index.
toPage :: Entry -> Int -> Page
toPage Entry {..} index = Page index title ext
    where title = dropExtension eRelativePath
          ext   = drop 1 (takeExtension eRelativePath)

-- | Returns whether or not the given page is for an image.
isImage :: Page -> Bool
isImage (Page _ _ ext) = elem ext FileType.validImageTypes
