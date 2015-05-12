module Hash where

import Crypto.Hash.MD5                      (hash)
import Data.Functor                         ((<$>))
import Text.Printf                          (printf)
import qualified Data.ByteString as Byte    (readFile, unpack)

-- | Calculates the MD5 hash of a file as a hexidecimal string.
hashFile :: FilePath -> IO String
hashFile x = concatMap toHex <$> Byte.unpack <$> hash <$> Byte.readFile x
             where toHex = printf "%02x"
