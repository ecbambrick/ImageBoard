module Graphics.Thumbnail where

import System.Directory ( createDirectoryIfMissing )
import System.FilePath  ( takeDirectory, takeExtension, replaceExtension )
import System.Process   ( runCommand, waitForProcess )
import Text.Printf      ( printf )

-- | Generates a thumbnail of the given size from the first given file path and
-- | save it to the second given file path.
createThumbnail :: Int -> FilePath -> FilePath -> IO ()
createThumbnail size from to = do

    let cmd   = printf (unwords text) size size
        from' = if takeExtension from == ".gif" then from ++ "[0]" else from
        to'   = replaceExtension to "jpg"
        text  = [ "convert"
                , "-background white"
                , "-format jpg"
                , "-thumbnail \"%dx%d\""
                , "\"" ++ from' ++ "\""
                , "\"" ++ to'   ++ "\"" ]

    createDirectoryIfMissing True (takeDirectory to)
    waitForProcess =<< runCommand cmd
    return ()
