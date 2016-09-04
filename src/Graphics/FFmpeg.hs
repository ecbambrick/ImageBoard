module Graphics.FFmpeg where

import qualified Data.ByteString  as ByteString
import qualified System.Directory as Dir
import qualified System.FilePath  as FilePath
import qualified System.IO        as IO
import qualified System.Process   as Process

import Control.Monad    ( void )
import System.Process   ( CreateProcess(..), StdStream(..) )
import Text.Regex       ( matchRegex, mkRegex )

-- | Generates a thumbnail with a maximum dimension of the given size for the
-- | image or video file at the first given file path and save it to the second
-- | given file path. If the second path does not exist, it will be created.
-- | Any existing file will be overwritten.
createThumbnail :: Int -> FilePath -> FilePath -> IO ()
createThumbnail size from to = do
    let process = (Process.proc name args) { std_out = CreatePipe, std_err = CreatePipe }
        name    = "ffmpeg"
        width   = "'if(gt(iw,ih),512,-1)'"
        height  = "'if(gt(iw,ih),-1,512)'"
        args    = [ "-i", from
                  , "-y"
                  , "-vframes", "1"
                  , "-vf", "scale=" ++ width ++ ":" ++ height
                  , to ]

    Dir.createDirectoryIfMissing True (FilePath.takeDirectory to)
    (_, _, _, handle) <- Process.createProcess process
    void $ Process.waitForProcess handle

-- | Returns the width and height of the given image or video file.
getDimensions :: FilePath -> IO (Int, Int)
getDimensions path = do
    let process = (Process.proc name args) { std_out = CreatePipe, std_err = CreatePipe }
        name    = "ffprobe"
        args    = [ "-show_entries"
                  , "stream=width,height"
                  , path ]

    (_, Just stdout, _, handle) <- Process.createProcess process
    results                     <- IO.hGetContents stdout
    Process.waitForProcess handle

    let Just [width]  = mkRegex "^width=([0-9]+)$"  `matchRegex` results
        Just [height] = mkRegex "^height=([0-9]+)$" `matchRegex` results

    return (read width, read height)
