module App.Import where

import qualified App.Core.Album as Album
import qualified App.Core.Image as Image

import App.Config           ( loadConfig )
import App.FileType         ( FileType(..), getFileType )
import Control.Monad.Reader ( void, forM_, runReaderT )
import System.FilePath      ( (</>), dropExtension )
import System.Directory     ( getDirectoryContents )

-- | Import each valid file in the given directory into the database.
fromDirectory :: FilePath -> IO ()
fromDirectory path = do
    config <- loadConfig
    files  <- getDirectoryContents path

    flip runReaderT config $ do
        forM_ files $ \fileName ->
            let title    = dropExtension fileName
                filePath = path </> fileName

            in case (getFileType filePath fileName) of
                ArchiveType file -> void $ Album.insert file title []
                ImageType   file -> void $ Image.insert file title []
                InvalidType _    -> return ()
