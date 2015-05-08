{-# LANGUAGE OverloadedStrings #-}
module Database.Tag where

import Data.Functor             ((<$>))
import Data.Maybe               (listToMaybe)
import Database.SQLite.Simple   (Connection(..), Only(..), query, query_)
import Types                    (Image(..), Tag(..), ID)

-- | Gets a tag with the given ID from the database or nothing if no tag exists.
getTagByID :: Connection -> ID -> IO (Maybe Tag)
getTagByID connection id = listToMaybe <$> query connection command (Only id)
    where command = "SELECT tag.id, tag.name \
                    \FROM tag \
                    \WHERE id = ?"
 
-- | Gets all tags attached to the image with the given ID from the database.
getTagsByImageID :: Connection -> ID -> IO [Tag]
getTagsByImageID connection id = query connection command (Only id)
    where command = "SELECT tag.id, tag.name \
                    \FROM tag \
                    \JOIN image_tag ON image_tag.tagId = tag.id \
                    \WHERE image_tag.imageId = ?"

-- | Gets all tags from the database.
getAllTags :: Connection -> IO [Tag]
getAllTags connection = query_ connection command
    where command = "SELECT tag.id, tag.name \
                    \FROM tag"
