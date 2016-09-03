{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Post where

import qualified App.Core.Image     as Image
import qualified App.Core.Album     as Album
import qualified System.IO.Metadata as Metadata

import App.Core.Types      ( App )
import App.Validation      ( Error(..), Validation(..) )
import Control.Monad.Trans ( liftIO )

------------------------------------------------------------------------- Types

-- | The type of post that was inserted.
data PostType = ImagePost | AlbumPost

-------------------------------------------------------------------------- CRUD

-- | Inserts a new post into the database/filesystem based on the given file
-- | path, title and tags. Whether the post is an image or album is dependent
-- | on the MIME type. Returns the insertion type as well as valid if the
-- | insertion was sucessful; otherwise invalid.
insert :: FilePath -> String -> [String] -> App (PostType, Validation)
insert path title tags = do
    mimeType <- liftIO $ Metadata.getMIMEType path

    case mimeType of
        Just ("image",    ext) -> (,) ImagePost <$> Image.insert path ext title tags
        Just ("video", "webm") -> (,) ImagePost <$> Image.insert path "webm" title tags
        Just (_,        "zip") -> (,) AlbumPost <$> Album.insert path title tags
        _                      -> (,) AlbumPost <$> return (Invalid [UnrecognizedFile])
