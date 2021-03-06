{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Post ( PostType(..), canInsert, insert ) where

import qualified App.Core.Image     as Image
import qualified App.Core.Album     as Album
import qualified System.IO.Metadata as Metadata

import App.Core.Types ( App, URL )
import App.Validation ( Error(..), Result(..), Validation )
import Data.Textual   ( trim )

------------------------------------------------------------------------- Types

-- | The type of post insertion.
data PostType = InvalidPost [Error] | ImagePost | AlbumPost

---------------------------------------------------------------------- Commands

-- | Returns whether or not the file with the given path can be inserted based
-- | on the file type.
canInsert :: FilePath -> App Bool
canInsert path = do
    mimeType <- Metadata.getMIMEType path

    case mimeType of
        Just ("image", _) -> return True
        Just ("video", _) -> return True
        Just (_,   "zip") -> return True
        _                 -> return False

-- | Inserts a new post into the database/filesystem based on the given file
-- | path, title, sources and tags. Whether the post is an image or album is
-- | dependent on the file's MIME type. Returns the post insertion type.
insert :: FilePath -> String -> [URL] -> [String] -> App PostType
insert path title urls tags = do
    mimeType <- Metadata.getMIMEType path

    case mimeType of
        Just ("image", ext) -> imagePost <$> Image.insert path ext title urls tags
        Just ("video", ext) -> imagePost <$> Image.insert path ext title urls tags
        Just (_,     "zip") -> albumPost <$> Album.insert path     title urls tags
        _                   -> badPost   <$> return UnrecognizedFile

----------------------------------------------------------------------- Utility

-- Converts the given validation to an invalid post or image post.
imagePost :: Validation () -> PostType
imagePost (Success _) = ImagePost
imagePost (Failure e) = InvalidPost e

-- Converts the given validation to an invalid post or album post.
albumPost :: Validation () -> PostType
albumPost (Success _) = AlbumPost
albumPost (Failure e) = InvalidPost e

-- Converts the given error to an invalid post.
badPost :: Error -> PostType
badPost e = InvalidPost [e]
