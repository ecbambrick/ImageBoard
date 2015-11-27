{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Album 
    ( count, delete, getPage, insert, query, querySingle, update ) where

import qualified App.Core.Tag as Tag
import qualified Data.ByteString as ByteString

import App.Common           ( Album(..), Page(..), Tag(..), App, runDB )
import App.Config           ( Config(..) )
import App.Database         ( deleteAlbum, insertAlbum, selectAlbum
                            , selectAlbums, updateAlbum, selectAlbumsCount
                            , attachTags, detachTags, cleanTags )
import App.Expression       ( Expression )
import App.FileType         ( ArchiveFile, File(..) )
import App.Paths            ( getAlbumPath, getAlbumThumbnailPath, getPagePath
                            , getPageThumbnailPath )
import App.Validation       ( Error(..), Validation(..), isValid, verify )
import Codec.Archive.Zip    ( Archive(..), Entry(..), toArchive, fromEntry )
import Control.Applicative  ( (<$>) )
import Control.Monad        ( when )
import Control.Monad.Trans  ( liftIO )
import Control.Monad.Reader ( asks )
import Data.ByteString.Lazy ( hGetContents, hPut )
import Data.List            ( (\\), sortBy, find )
import Data.Maybe           ( isJust, fromJust )
import Data.Monoid          ( (<>), mconcat )
import Data.Ord.Extended    ( comparingAlphaNum )
import Data.Time            ( getCurrentTime )
import Database.Engine      ( Entity(..), ID, fromEntity )
import Graphics.Thumbnail   ( createThumbnail )
import System.FilePath      ( takeBaseName, takeExtension )
import System.Directory     ( createDirectoryIfMissing, doesDirectoryExist
                            , removeDirectoryRecursive )
import System.IO            ( IOMode(..), hClose, openFile, withFile )

-------------------------------------------------------------------------- CRUD

-- | Returns the total number of albums satisfying the given expression.
count :: Expression -> App Int
count = runDB . selectAlbumsCount

-- | Deletes the album with the given ID from the database/filesystem.
delete :: ID -> App ()
delete id = do
    path       <- getAlbumPath id
    pathExists <- liftIO $ doesDirectoryExist path

    runDB $ do
        deleteAlbum id
        cleanTags
    
    liftIO $ do
        when pathExists (removeDirectoryRecursive path)

-- | Returns the page with the given number from the given album.
getPage :: Album -> Int -> Maybe Page
getPage Album {..} number = find (\x -> number == pageNumber x) albumPages
    
-- | Inserts a new album into the database/filesystem based on the given file,
-- | title and tags. Returns valid if the insertion was sucessful; otherwise 
-- | invalid.
insert :: ArchiveFile -> String -> [String] -> App Validation
insert file title tagNames = do
    handle  <- liftIO $ openFile (getPath file) ReadMode
    archive <- liftIO $ toArchive <$> hGetContents handle
    now     <- liftIO $ getCurrentTime
     
    let entries    = sortBy (comparingAlphaNum eRelativePath) (zEntries archive)
        anyEntries = not (null entries)
        entryPairs = zip entries [1..length entries]
        pages      = map (uncurry toPage) entryPairs
        fileSize   = sum $ map (fromIntegral . eUncompressedSize) entries
        tags       = Tag.cleanTags tagNames
        album      = Album title False now now fileSize pages tags
        results    = validate album
    
    when (isValid results) $ do
        id <- runDB $ do
            id <- insertAlbum album
            attachTags tags id
            return id
        
        when anyEntries $ do
            basePath  <- getAlbumPath id
            firstPath <- getPagePath id (toPage (head entries) 1)
            thumbPath <- getAlbumThumbnailPath id
            thumbSize <- asks configThumbnailSize
            
            liftIO $ createDirectoryIfMissing True basePath
            mapM_ (extractFile id) entryPairs
            liftIO $ createThumbnail thumbSize firstPath thumbPath
        
    liftIO $ hClose handle
    
    return results

-- | Returns a page of albums based on the given page number and filter.
query :: Expression -> Int -> App [Entity Album]
query expression page = do
    size <- asks configPageSize
    runDB $ selectAlbums expression ((page - 1) * size) size

-- | Returns the album with the given ID.
querySingle :: ID -> App (Maybe (Entity Album))
querySingle = runDB . selectAlbum

-- | Updates the given album in the database. Returns valid if the update was
-- | successful; otherwise, invalid.
update :: Entity Album -> App Validation
update (Entity id album) = do
    now      <- liftIO $ getCurrentTime
    previous <- runDB  $ selectAlbum id
    
    let isFound = verify (isJust previous) (Error "id" (show id) "ID not found") 
        results = validate album <> isFound
    
    when (isValid results) $ runDB $ do
        let newTags = albumTagNames album
            oldTags = albumTagNames . fromEntity . fromJust $ previous
    
        updateAlbum (Entity id album { albumModified = now })
        detachTags (oldTags \\ newTags) id
        attachTags (newTags \\ oldTags) id
        cleanTags
    
    return results

----------------------------------------------------------------------- Utility

-- | Returns valid if all fields of the given album are valid; otherwise 
-- | invalid. Any validation that requires access to the database is ignored.
validate :: Album -> Validation
validate Album {..} = mconcat (Tag.validate . Tag <$> albumTagNames)

-- | Extracts the given zip file entry.
extractFile :: ID -> (Entry, Int) -> App ()
extractFile id (entry, index) = do
    let contents = fromEntry entry
        page     = toPage entry index
        
    extractPath   <- getPagePath id page
    thumbnailPath <- getPageThumbnailPath id page
    thumbSize     <- asks configThumbnailSize
        
    liftIO $ do
        withFile extractPath WriteMode (flip hPut contents)
        createThumbnail thumbSize extractPath thumbnailPath

-- | Creates a new page from the given zip file entry and index. 
toPage :: Entry -> Int -> Page
toPage Entry {..} index = Page title index ext
    where title = takeBaseName eRelativePath
          ext   = tail (takeExtension eRelativePath)
