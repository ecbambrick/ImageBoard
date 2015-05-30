{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.SQLite.Entities ( deleteImage, insertImage, selectImage
                                , selectImages, selectTags, selectTagsByImage
                                , updateImage, attachTag, attachTags, clearTags
                                , cleanTags, withDatabase ) where

import Common                   ( Config(..), Entity(..), Image(..), Tag(..)
                                , ID, App, (<$$>) )
import Control.Applicative      ( (<$>), (<*>) )
import Control.Monad            ( when )
import Control.Monad.IO.Class   ( liftIO )
import Control.Monad.Reader     ( asks )
import Data.Int                 ( Int64 )
import Data.Maybe               ( listToMaybe )
import Data.Text                ( pack )
import Database.SQLite.Simple   ( FromRow(..), Query(..), Connection
                                , lastInsertRowId, execute, execute_, field
                                , query, query_, withConnection
                                , withTransaction )
import Foreign.Marshal.Utils    ( toBool, fromBool )



------------------------------------------------------------------------- Types

-- | A database-specific representation of an image.
data DBImage = DBImage ID String Int String String Int Int String String Int

-- | A database-specific representation of a tag.
data DBTag = DBTag ID String

instance FromRow Int64 where
    fromRow = field

instance FromRow Int where
    fromRow = field
    
instance FromRow DBTag where
    fromRow = DBTag <$> field <*> field
    
instance FromRow DBImage where
    fromRow = DBImage <$> field <*> field <*> field <*> field <*> field
                      <*> field <*> field <*> field <*> field <*> field

------------------------------------------------------------------------ Images

-- | Deletes the image with the given ID. If no image exists, nothing happens.
deleteImage :: Connection -> ID -> IO ()
deleteImage conn id = execute conn command [id]
    where command = "DELETE FROM post WHERE id = ?"

-- | Inserts a new image into the database and returns its ID.
insertImage :: Connection -> Image -> IO ID
insertImage conn image = do
    let (DBImage _ title fav hash ext w h _ _ size) = fromImage image
    execute conn postCommand (title, fav)
    postID <- lastInsertRowId conn
    execute conn imageCommand (postID, hash, w, h, size, ext)
    return postID
    where
        postCommand  = "INSERT INTO post \
                       \(title, is_favourite) \
                       \VALUES (?, ?);"

        imageCommand = "INSERT INTO image \
                       \(post_id, hash, width, height, file_size, extension) \
                       \VALUES (?, ?, ?, ?, ?, ?);"

-- | Returns the image from the database with the given ID. If no image exists,
-- | nothing is returned.
selectImage :: Connection -> ID -> IO (Maybe (Entity Image))
selectImage conn id = listToMaybe <$> toImage <$$> query conn command [id]
    where command = "SELECT p.id, p.title, p.is_favourite, i.hash, \
                    \i.extension, i.width, i.height, p.created, p.modified, \
                    \i.file_size \
                    \FROM post p \
                    \INNER JOIN image i ON i.post_id = p.id \
                    \WHERE i.id = ?;"

-- | Returns a list of all images from the database.
selectImages :: Connection -> IO [Entity Image]
selectImages conn = toImage <$$> query_ conn command
    where command = "SELECT p.id, p.title, p.is_favourite, i.hash, \
                    \i.extension, i.width, i.height, p.created, p.modified, \
                    \i.file_size \
                    \FROM post p \
                    \INNER JOIN image i ON i.post_id = p.id \
                    \ORDER BY p.id;"

-- | Updates the image in the database.
updateImage :: Connection -> Entity Image -> IO ()
updateImage conn (Entity id image) = do
    let (DBImage _ title fav _ _ _ _ _ modified size) = fromImage image
    execute conn command (title, fav, modified, id)
    where command = "UPDATE post SET \
                    \title = ?, \
                    \is_favourite = ?, \
                    \modified = ? \
                    \WHERE id = ?;"

-------------------------------------------------------------------------- Tags

-- | Returns a list of all tags from the database.
selectTags :: Connection -> IO [Entity Tag]
selectTags conn = toTag <$$> query_ conn command
    where command = "SELECT id, name FROM tag ORDER BY name;"

-- | Returns a list of all tags associated to the image with the given ID.
selectTagsByImage :: Connection -> ID -> IO [Entity Tag]
selectTagsByImage conn id = toTag <$$> query conn command [id]
    where command = "SELECT t.id, t.name \
                    \FROM tag t \
                    \INNER JOIN post_tag pt ON pt.tag_id = t.id \
                    \WHERE pt.post_id = ? \
                    \ORDER BY name;"

-- | Deletes all tags from the database that are not associated to any image.
cleanTags :: Connection -> IO ()
cleanTags conn = do
    orphanTags <- query_ conn (Query $ pack command1) :: IO [ID]
    mapM_ (\x -> execute conn command2 [x]) orphanTags
    where
        command1 = "SELECT id FROM tag WHERE NOT EXISTS (" ++ subquery ++ ");"
        subquery = "SELECT * FROM post_tag WHERE tag_id = tag.id"
        command2 = "DELETE FROM tag WHERE id = ?;"

----------------------------------------------------------------- Relationships

-- | Associates the image with the given ID with the tag with the given name.
-- | If the tag name does not exist, it is added to the database.
attachTag :: Connection -> ID -> String -> IO ()
attachTag conn postID name = do
    postExists <- not . null <$> getPost
    when postExists $ do
        tagID <- getTagID name
        case tagID of
            Nothing -> insert name >>= attach postID
            Just id -> attach postID id
    where
        getTagID a = listToMaybe <$> query conn selectCmd [a] :: IO (Maybe ID)
        insert a   = execute conn insertCmd [a] >> lastInsertRowId conn
        attach a b = execute conn attachCmd (a, b)
        getPost    = query conn getPstCmd [postID] :: IO [ID]
        selectCmd  = "SELECT id FROM tag WHERE name = ?;"
        insertCmd  = "INSERT INTO tag (name) VALUES (?);"
        attachCmd  = "INSERT INTO post_tag (post_id, tag_id) VALUES (?, ?);"
        getPstCmd  = "SELECT id FROM post WHERE id = ?";

-- | Associates the image with the given ID with all tags that map to the given
-- | list of names. If any tag does not eixsts, it is added to the database.
attachTags :: Connection -> ID -> [String] -> IO ()
attachTags conn postID names = mapM_ (attachTag conn postID) names

-- | Disassociates all tags from the image with the given ID.
clearTags :: Connection -> ID -> IO ()
clearTags conn id = execute conn deleteCmd [id]
    where deleteCmd = "DELETE FROM post_tag WHERE post_id = ?;"

----------------------------------------------------------------------- Utility

-- | Transforms an Image into a DBImage.
fromImage :: Image -> DBImage
fromImage (Image title fav hash ext w h created modified size _) =
    (DBImage 0 title (fromBool fav) hash ext w h created modified size)

-- | Transforms a DBImage into an Entity Image.
toImage :: DBImage -> Entity Image
toImage (DBImage id title fav hash ext w h created modified size) =
    Entity id (Image title (toBool fav) hash ext w h created modified size [])

-- | Transforms a DBTag into an Entity Tag.
toTag :: DBTag -> Entity Tag
toTag (DBTag id name) = Entity id (Tag name)

-- | Applies the given function within a database transaction.
withDatabase :: (Connection -> IO a) -> App a
withDatabase f = do
    database <- asks configDatabaseConnection
    liftIO $ withConnection database $ \conn -> do
        execute_ conn "PRAGMA foreign_keys = ON;"
        withTransaction conn $ f conn
