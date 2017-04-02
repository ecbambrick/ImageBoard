{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Album
    ( count, query, querySingle
    , delete, insert, update
    , getPage
    ) where

import qualified App.Config             as Config
import qualified App.Core.Tag           as Tag
import qualified App.Core.Title         as Title
import qualified App.Storage.Database   as DB
import qualified App.Storage.FileSystem as FileSystem
import qualified App.Storage.Path       as Path
import qualified App.Validation         as Validation
import qualified Data.ByteString        as ByteString
import qualified Data.DateTime          as DateTime
import qualified Graphics.FFmpeg        as Graphics
import qualified System.IO              as IO
import qualified System.IO.Metadata     as Metadata

import App.Control          ( runDB )
import App.Core.Types       ( Album(..), DeletionMode(..), Page(..), App, ID )
import App.Expression       ( Expression )
import App.Validation       ( Error(..), Validation )
import Codec.Archive.Zip    ( Archive(..), Entry(..), toArchive, fromEntry )
import Control.Monad        ( void )
import Control.Monad.Trans  ( liftIO )
import Data.ByteString.Lazy ( hGetContents, hPut )
import Data.List            ( (\\), sortBy, find )
import Data.Maybe           ( fromJust, isJust )
import Data.Ord.Extended    ( comparingAlphaNum )
import System.FilePath      ( dropExtension, takeExtension )
import System.IO            ( IOMode(..), Handle )

----------------------------------------------------------------------- Queries

-- | Returns the total number of albums satisfying the given expression.
count :: Expression -> App Int
count = runDB . DB.selectAlbumsCount

-- | Returns a page of albums satisfying the given expression.
query :: Expression -> Int -> App [Album]
query expression page = do
    size <- Config.pageSize
    runDB $ DB.selectAlbums expression ((page - 1) * size) size

-- | Returns the album with the given ID.
querySingle :: ID -> App (Maybe Album)
querySingle = runDB . DB.selectAlbum

---------------------------------------------------------------------- Commands

-- | Deletes the album with the given ID. If the delete is permanent, it is
-- | removed from the database/file system; otherwise, it is just marked as
-- | deleted.
delete :: DeletionMode -> ID -> App ()
delete mode albumID =
    case mode of
        MarkAsDeleted -> do
            runDB $ DB.markPostAsDeleted albumID

        PermanentlyDelete -> do
            runDB $ DB.deletePost albumID
            FileSystem.deleteAlbum albumID

-- | Inserts a new album into the database/filesystem based on the given file
-- | path, title and tags. Returns valid if the insertion was sucessful;
-- | otherwise, invalid.
insert :: FilePath -> String -> [String] -> App (Validation ())
insert path title tags = do
    file    <- liftIO $ IO.openFile path ReadMode
    entries <- liftIO $ getImages file
    now     <- DateTime.now

    let pages = map (uncurry toPage) (zip entries [1..])
        size  = sum $ map (fromIntegral . eUncompressedSize) entries

    let result = Album <$> pure 0
                       <*> Title.validate title
                       <*> pure False
                       <*> pure now
                       <*> pure now
                       <*> pure size
                       <*> pure pages
                       <*> Tag.validateNames tags
                       <*  Validation.reject (null entries) [EmptyAlbum]

    Validation.whenSuccess result $ \album -> do
        let archiveData = map fromEntry entries
            pageData    = zip pages archiveData

        albumID <- runDB $ do
            albumID <- DB.insertAlbum album
            DB.attachTags (albumTags album) albumID
            return albumID
        FileSystem.addAlbum albumID pageData

    liftIO $ IO.hClose file

    return (void result)

-- | Updates the given album in the database. Returns valid if the update was
-- | successful; otherwise, invalid.
update :: ID -> String -> [String] -> App (Validation ())
update albumID title tags = do
    now      <- DateTime.now
    existing <- runDB $ DB.selectAlbum albumID

    let result = (,) <$> Title.validate title
                     <*> Tag.validateNames tags
                     <*  Validation.assert (isJust existing) [IDNotFound albumID]

    Validation.whenSuccess result $ \(title, tags) -> do
        let newTags  = albumTags newAlbum
            oldTags  = albumTags oldAlbum
            oldAlbum = fromJust existing
            newAlbum = oldAlbum { albumTags     = tags
                                , albumTitle    = title
                                , albumModified = now }

        runDB $ do
            DB.updateAlbum newAlbum
            DB.detachTags (oldTags \\ newTags) albumID
            DB.attachTags (newTags \\ oldTags) albumID

    return (void result)

----------------------------------------------------------------------- Utility

-- | Returns the page with the given number from the given album.
getPage :: Album -> Int -> Maybe Page
getPage Album {..} number = find (\x -> number == pageNumber x) albumPages

-- | Creates a new page from the given zip file entry and index.
toPage :: Entry -> Int -> Page
toPage Entry {..} index =
    let title = dropExtension eRelativePath
        ext   = drop 1 (takeExtension eRelativePath)

    in Page index title ext

-- | Gets the list of image entries from the given archive.
getImages :: Handle -> IO [Entry]
getImages file = do
    archive <- toArchive <$> hGetContents file

    let entries = sortBy (comparingAlphaNum eRelativePath) (zEntries archive)

    return $ flip filter entries $ \entry ->
        let mimeType = Metadata.getMIMETypeFromBytes (fromEntry entry)
        in case mimeType of
            Just ("image",      _) -> True
            Just ("video", "webm") -> True
            _                      -> False
