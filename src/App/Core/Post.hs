{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Post ( PostType(..), insert ) where

import qualified App.Core.Image     as Image
import qualified App.Core.Album     as Album
import qualified System.IO.Metadata as Metadata

import App.Core.Types ( App )
import App.Validation ( Error(..), Validation(..) )

------------------------------------------------------------------------- Types

-- | The type of post insertion.
data PostType = InvalidPost Validation | ImagePost | AlbumPost

-------------------------------------------------------------------------- CRUD

-- | Inserts a new post into the database/filesystem based on the given file
-- | path, title and tags. Whether the post is an image or album is dependent
-- | on the file's MIME type. Returns the post insertion type.
insert :: FilePath -> String -> [String] -> App PostType
insert path title tags = do
    mimeType <- Metadata.getMIMEType path

    case mimeType of
        Just ("image", ext) -> imagePost <$> Image.insert path ext title tags
        Just ("video", ext) -> imagePost <$> Image.insert path ext title tags
        Just (_,     "zip") -> albumPost <$> Album.insert path title tags
        _                   -> badPost   <$> return UnrecognizedFile

----------------------------------------------------------------------- Utility

-- Converts the given validation to an invalid post or image post.
imagePost :: Validation -> PostType
imagePost Valid = ImagePost
imagePost e     = InvalidPost e

-- Converts the given validation to an invalid post or album post.
albumPost :: Validation -> PostType
albumPost Valid = AlbumPost
albumPost e     = InvalidPost e

-- Converts the given error to an invalid post.
badPost :: Error -> PostType
badPost e = InvalidPost (Invalid [e])
