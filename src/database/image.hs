{-# LANGUAGE OverloadedStrings #-}
module Database.Image where

import Control.Monad            (forM, when)
import Data.List                (nub)
import Data.Functor             ((<$>))
import Data.Maybe               (isJust, fromJust)
import Data.Text                (pack)
import Database.SQLite.Simple   (Connection, Only(..), query, query_, execute, lastInsertRowId)
import Database.Tag             (getTagByID, getTagsByImageID, setTag)
import Types                    (Image(..), Tag(..), ID)

-- | Associates the tag with the given ID to the image with the given ID.
attachTag :: Connection -> ID -> ID -> IO ()
attachTag connection imageID tagID = do
    tagExists   <- isJust <$> getTagByID connection tagID
    imageExists <- isJust <$> getImageByID connection imageID
    when (tagExists && imageExists) $ execute connection command (imageID, tagID)
    where command = "INSERT OR IGNORE INTO image_tag \
                    \(imageId, tagId) \
                    \VALUES \
                    \(?, ?)"

-- | Disassociates all tags from the image with the given ID.
clearTags :: Connection -> ID -> IO ()
clearTags connection id = execute connection command (Only id)
    where command = "DELETE FROM image_tag \
                    \WHERE imageId = ?"

-- | Deletes the image with the given ID from the database.
deleteImage :: Connection -> ID -> IO ()
deleteImage connection id = execute connection command (Only id)
    where command = "DELETE FROM image \
                    \WHERE id = ?"

-- | Disassociates the tag with the given ID from the image with the given ID.
detachTag :: Connection -> ID -> ID -> IO ()
detachTag connection imageID tagID = do
    tagExists   <- isJust <$> getTagByID connection tagID
    imageExists <- isJust <$> getImageByID connection imageID
    when (tagExists && imageExists) $ execute connection command (imageID, tagID)
    where command = "DELETE FROM image_tag \
                    \WHERE imageId = ? AND tagId = ?"

-- | Gets all images from the database.
getAllImages :: Connection -> IO [Image]
getAllImages connection = query_ connection command >>= mapM includeTags
    where includeTags image = do
              tags <- getTagsByImageID connection $ fromJust $ _imageID image
              return image { _imageTags = tags }
    
          command = "SELECT id, title, extension, hash \
                    \FROM image"

-- | Gets the image with the given ID or nothing if no image exists.
getImageByID :: Connection -> ID -> IO (Maybe Image)
getImageByID connection id = do
    results <- query connection command (Only id)
    case results of
        []   -> return Nothing
        x:xs -> do
            tags <- getTagsByImageID connection $ fromJust $ _imageID x
            return (Just x { _imageTags = tags })
            
    where command = "SELECT id, title, extension, hash \
                    \FROM image WHERE id = ?"

-- | Inserts or updates an image and returns a copy of that image containing 
-- | any updated data from the database.
setImage :: Connection -> Image -> IO Image
setImage connection image@(Image id _ _ _ _) = case id of
    Just id -> updateImage connection image
    Nothing -> insertImage connection image
    where
    
        insertImage connection (Image _ title ext hash tags) = do
            execute connection insertCommand (title, ext, hash)
            id <- lastInsertRowId connection
            attachTagsByName id title ext hash tags
            where insertCommand = "INSERT INTO image \
                                  \(title, extension, hash) \
                                  \VALUES \
                                  \(?, ?, ?)"
        
        updateImage connection (Image (Just id) title ext hash tags) = do
            execute connection updateCommand (title, ext, hash, id)
            attachTagsByName id title ext hash tags
            where updateCommand = "UPDATE image SET \
                                  \title = ?, \
                                  \extension = ?, \
                                  \hash = ? \
                                  \WHERE id = ?"
        
        attachTagsByName id title ext hash tags = do
            tags' <- nub <$> (forM tags $ setTag connection)
            forM tags' $ attachTag connection id . fromJust . _tagID
            return (Image (Just id) title ext hash tags')
