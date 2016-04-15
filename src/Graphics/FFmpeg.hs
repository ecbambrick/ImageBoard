module Graphics.FFmpeg where

import qualified Data.ByteString as ByteString

import Control.DeepSeq  ( deepseq )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath  ( takeDirectory )
import System.Process   ( CreateProcess(..), StdStream(..), createProcess, proc )
import System.IO        ( hClose, hGetContents )
import Text.Regex       ( matchRegex, mkRegex )

-- | Generates a thumbnail with a maximum dimension of the given size for the
-- | image or video file at the first given file path and save it to the second
-- | given file path. If the second path does not exist, it will be created.
-- | Any existing file will be overwritten.
createThumbnail :: Int -> FilePath -> FilePath -> IO ()
createThumbnail size from to = do
    let process = (proc name args) { std_out = CreatePipe, std_err = CreatePipe }
        name    = "ffmpeg"
        args    = [ "-i", from
                  , "-y"
                  , "-vframes", "1"
                  , "-filter:v", "scale=min(" ++ show size ++ "\\, iw):-1"
                  , to ]

    createDirectoryIfMissing True (takeDirectory to)

    -- Evaluate output to prevent bug where thumbnails don't get generated.
    (_, _, Just stderr, _) <- createProcess process
    ByteString.hGetContents stderr
    hClose stderr

-- | Returns the width and height of the given image or video file.
getDimensions :: FilePath -> IO (Int, Int)
getDimensions path = do
    let process = (proc name args) { std_out = CreatePipe, std_err = CreatePipe }
        name    = "ffprobe"
        args    = [ "-show_entries"
                  , "stream=width,height"
                  , path ]

    (_, Just stdout, _, _) <- createProcess process
    results                <- hGetContents stdout
    results `deepseq` hClose stdout

    let Just [width]  = mkRegex "^width=([0-9]+)$"  `matchRegex` results
        Just [height] = mkRegex "^height=([0-9]+)$" `matchRegex` results

    return (read width, read height)
