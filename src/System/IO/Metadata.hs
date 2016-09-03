{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.IO.Metadata
    ( MIMEType, getHash, getMIMEType, getMIMETypeFromBytes, getSize ) where

import qualified Crypto.Hash.MD5      as MD5
import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified System.Directory     as Dir
import qualified System.IO            as IO

import Data.ByteString.Lazy ( ByteString )
import Data.Functor         ( (<$>) )
import Data.Int             ( Int64 )
import Data.Maybe           ( catMaybes, listToMaybe )
import Text.Printf          ( printf )
import System.IO            ( IOMode(..) )

------------------------------------------------------------------------- Types

-- A MIME type containing a type and subtype.
type MIMEType = (String, String)

---------------------------------------------------------------------- Metadata

-- | Returns the MD5 hash of the given file as a string of hexidecimals.
getHash :: FilePath -> IO String
getHash path =
    let toHexidecimal = concatMap (printf "%02x")
    in toHexidecimal <$> Strict.unpack <$> MD5.hash <$> Strict.readFile path

-- | Returns the mime type of the given file as a string.
getMIMEType :: FilePath -> IO (Maybe MIMEType)
getMIMEType path = do
    validFile <- Dir.doesFileExist path
    if not validFile
        then return Nothing
        else IO.withFile path ReadMode $ \file ->
            getMIMETypeFromBytes <$> Lazy.fromStrict <$> Strict.hGet file 35

-- | Returns the mime type of the given file as a string.
getMIMETypeFromBytes :: ByteString -> Maybe MIMEType
getMIMETypeFromBytes bytes =
    listToMaybe $ catMaybes
        [ parseBytes 0  4 "GIF8"    ("image", "gif" )      bytes
        , parseBytes 6  4 "JFIF"    ("image", "jpeg")      bytes
        , parseBytes 6  4 "Exif"    ("image", "jpeg")      bytes
        , parseBytes 0  4 "\137PNG" ("image", "png" )      bytes
        , parseBytes 31 4 "webm"    ("video", "webm")      bytes
        , parseBytes 0  4 "PK\5\6"  ("application", "zip") bytes
        , parseBytes 0  4 "PK\3\4"  ("application", "zip") bytes
        , parseBytes 0  2 "BM"      ("image", "bmp" )      bytes ]

-- | Returns the size in bytes of the given file.
getSize :: FilePath -> IO Integer
getSize path = IO.withFile path ReadMode IO.hFileSize

----------------------------------------------------------------------- Utility

-- | Parses the last given byte string by skipping the given offset and taking
-- | the given length of bytes. If the first given byte string matches the
-- | results then the MIME type is returned; otherwise, nothing is returned.
parseBytes :: Int64 -> Int64 -> ByteString -> MIMEType -> ByteString -> Maybe MIMEType
parseBytes offset len magicNumber (major, minor) bytes =
    let header = Lazy.take len $ Lazy.drop offset bytes
    in if (header == magicNumber)
        then Just (major, minor)
        else Nothing
