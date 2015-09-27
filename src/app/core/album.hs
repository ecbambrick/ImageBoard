{-# LANGUAGE RecordWildCards #-}

module App.Core.Album ( insert ) where

import qualified Data.ByteString as ByteString

import App.Common           ( Album(..), Page(..), App, runDB )
import App.Config           ( Config(..) )
import App.Database         ( insertAlbum )
import App.FileType         ( ArchiveFile, File(..) )
import App.Paths            ( albumPath, albumThumbnailPath, pagePath
                            , pageThumbnailPath )
import App.Validation       ( Validation(..) )
import Codec.Archive.Zip    ( Archive(..), Entry(..), toArchive, fromEntry )
import Control.Applicative  ( (<$>) )
import Control.Monad        ( when )
import Control.Monad.Trans  ( liftIO )
import Control.Monad.Reader ( asks )
import Data.ByteString.Lazy ( hGetContents, hPut )
import Data.List            ( sortBy )
import Data.Ord.Extended    ( comparingAlphaNum )
import Data.Time            ( getCurrentTime )
import Database.Engine      ( ID )
import Graphics.Thumbnail   ( createThumbnail )
import System.FilePath      ( takeBaseName, takeExtension )
import System.Directory     ( createDirectoryIfMissing )
import System.IO            ( IOMode(..), hClose, openFile, withFile )

-------------------------------------------------------------------------- CRUD
    
-- | Inserts a new album into the database/filesystem based on the given file,
-- | title and tags. Returns valid if the insertion was sucessful; otherwise 
-- | invalid.
insert :: ArchiveFile -> String -> [String] -> App Validation
insert file title tagNames = do
    handle    <- liftIO $ openFile (getPath file) ReadMode
    archive   <- liftIO $ toArchive <$> hGetContents handle
    now       <- liftIO $ getCurrentTime
     
    let entries    = sortBy (comparingAlphaNum eRelativePath) (zEntries archive)
        anyEntries = not (null entries)
        entryPairs = zip entries [1..length entries]
        pages      = map (uncurry toPage) entryPairs
        fileSize   = sum $ map (fromIntegral . eUncompressedSize) entries
        album      = Album title False now now fileSize pages
    
    id <- runDB (insertAlbum album)
    
    when anyEntries $ do
        basePath  <- albumPath id
        firstPath <- pagePath id (toPage (head entries) 1)
        thumbPath <- albumThumbnailPath id
        thumbSize <- asks configThumbnailSize
        
        liftIO $ createDirectoryIfMissing True basePath
        mapM_ (extractFile id) entryPairs
        liftIO $ createThumbnail thumbSize firstPath thumbPath
    
    liftIO (hClose handle)
    
    return Valid

----------------------------------------------------------------------- Utility

-- | Extracts the given zip file entry.
extractFile :: ID -> (Entry, Int) -> App ()
extractFile id (entry, index) = do
    let contents = fromEntry entry
        page     = toPage entry index
        
    extractPath   <- pagePath id page
    thumbnailPath <- pageThumbnailPath id page
    thumbSize     <- asks configThumbnailSize
        
    liftIO $ do
        withFile extractPath WriteMode (flip hPut contents)
        createThumbnail thumbSize extractPath thumbnailPath

-- | Creates a new page from the given zip file entry and index. 
toPage :: Entry -> Int -> Page
toPage Entry {..} index = Page title index ext
    where title = takeBaseName eRelativePath
          ext   = tail (takeExtension eRelativePath)
