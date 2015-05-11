{-# LANGUAGE OverloadedStrings #-}
module Database.Tag where

import Data.Functor             ((<$>))
import Data.Maybe               (listToMaybe)
import Database.SQLite.Simple   (Connection, Only(..), execute, query, query_, lastInsertRowId)
import Types                    (ID, Tag(..))

-- | Deletes the tag with the given ID from the database.
deleteTag :: Connection -> ID -> IO ()
deleteTag connection id = execute connection command (Only id)
    where command = "DELETE FROM tag \
                    \WHERE id = ?"

-- | Gets all tags from the database.
getAllTags :: Connection -> IO [Tag]
getAllTags connection = query_ connection command
    where command = "SELECT tag.id, tag.name \
                    \FROM tag"

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

-- | Gets a tag with the given ID from the database or nothing if no tag exists.
getTagByName :: Connection -> String -> IO (Maybe Tag)
getTagByName connection name = listToMaybe <$> query connection command (Only name)
    where command = "SELECT id, name \
                    \FROM tag \
                    \WHERE name = ?"

-- | Inserts or updates a tag and returns a copy of that image containing 
-- | any updated data from the database.
setTag :: Connection -> Tag -> IO Tag
setTag connection (Tag id name) = do
    existingTag <- getTagByName connection name
    case existingTag of
        Just (Tag id name) -> return $ Tag id name
        Nothing -> case id of
            Just id -> updateTag id name
            Nothing -> insertTag id name
    where
    
        insertTag id name = do
            execute connection insertCommand (Only name)
            id <- lastInsertRowId connection
            return (Tag (Just id) name)
            where insertCommand = "INSERT INTO tag \
                                  \(name) \
                                  \VALUES \
                                  \(?)" 
        
        updateTag id name = do
            execute connection updateCommand (name, id)
            return (Tag (Just id) name)
            where updateCommand = "UPDATE tag SET \
                                  \name = ? \
                                  \WHERE id = ? "
