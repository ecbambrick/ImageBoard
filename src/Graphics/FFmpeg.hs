module Graphics.FFmpeg where

import System.Directory  ( createDirectoryIfMissing )
import System.FilePath   ( takeDirectory )
import System.Process    ( callProcess, readProcess )
import Text.Regex        ( matchRegex, mkRegex )

-- | Generates a thumbnail of the given size for the image or video file at
-- | the first given file path and save it to the second given file path. If
-- | the path does not exist, it will be created. Any existing file will be
-- | overwritten.
createThumbnail :: Int -> FilePath -> FilePath -> IO ()
createThumbnail size from to = do
    createDirectoryIfMissing True (takeDirectory to)
    callProcess "ffmpeg"
        [ "-i", from
        , "-y"
        , "-vframes", "1"
        , "-filter:v", "scale=min(" ++ show size ++ "\\, iw):-1"
        , to ]

-- | Returns the width and height of the given image or video file.
getDimensions :: FilePath -> IO (Int, Int)
getDimensions path = do
    results <- readProcess "ffprobe" ["-show_entries", "stream=height,width", path] []

    let Just [w] = matchRegex (mkRegex "^width=([0-9]+)$")  results
        Just [h] = matchRegex (mkRegex "^height=([0-9]+)$") results

    return (read w, read h)
