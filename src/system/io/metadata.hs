module System.IO.Metadata where

import qualified Data.ByteString as Byte

import Codec.Picture        (DynamicImage(..), Image(..), readImage)
import Codec.Picture.Types  (dynamicMap)
import Control.Exception    (ErrorCall(..), throwIO)
import Crypto.Hash.MD5      (hash)
import Data.Functor         ((<$>))
import Text.Printf          (printf)
import System.IO            (IOMode(..), hFileSize, withFile)

-- | Returns the MD5 hash of the given file as a hexidecimal string.
getHash :: FilePath -> IO String
getHash path = concatMap toHex <$> Byte.unpack <$> hash <$> Byte.readFile path
    where toHex = printf "%02x"

-- | Returns the size in bytes of the given file.
getSize :: FilePath -> IO Integer
getSize path = withFile path ReadMode hFileSize

-- | Returns the width and height of the given file.
getDimensions :: FilePath -> IO (Int, Int)
getDimensions path = do
    image <- readImage path
    case image of
        Left  e -> throwIO $ ErrorCall e
        Right i -> return (dynamicMap imageWidth i, dynamicMap imageHeight i)
