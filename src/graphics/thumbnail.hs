module Graphics.Thumbnail where

import System.Directory ( createDirectoryIfMissing )
import System.FilePath  ( takeDirectory, takeExtension, replaceExtension )
import System.Process   ( runCommand, waitForProcess )
import Text.Printf      ( printf )

-- | Generates a thumbnail of the given size from the first given file path and 
-- | save it to the second given file path.
createThumbnail :: Int -> FilePath -> FilePath -> IO ()
createThumbnail size from to = do
    createDirectoryIfMissing True (takeDirectory to)
    waitForProcess =<< runCommand cmd
    return ()
    where 
        cmd  = printf text (size * 2) (size * 2) size size size size
        text = unwords 
            [ "convert"
            , "-define jpeg:size=%dx%d"
            , "-background white"
            , "-format jpg"
            , "-thumbnail \"%dx%d^\""
            , "-gravity center"
            , "-extent %dx%d"
            , if takeExtension from == ".gif" then from ++ "[0]" else from
            , replaceExtension to "jpg" ]
