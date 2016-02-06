module System.IO.Metadata where

import qualified Data.ByteString as ByteSring

import Crypto.Hash.MD5      ( hash )
import Data.Functor         ( (<$>) )
import Text.Printf          ( printf )
import System.IO            ( IOMode(..), hFileSize, withFile )

-- | Returns the MD5 hash of the given file as a string of hexidecimals.
getHash :: FilePath -> IO String
getHash path =
    let toHexidecimal = concatMap (printf "%02x")
    in toHexidecimal <$> ByteSring.unpack <$> hash <$> ByteSring.readFile path

-- | Returns the size in bytes of the given file.
getSize :: FilePath -> IO Integer
getSize path = withFile path ReadMode hFileSize
