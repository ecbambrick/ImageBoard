{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module App.DataSource.SQLite
    ( deleteImage, insertImage, selectHashExists, selectImage, selectImages
    , selectImagesByExpression, selectTags, selectTagsByImage, updateImage
    , attachTags, clearTags, cleanTags ) where
                                
import App.Common               ( Config(..), Entity(..), Image(..), Tag(..)
                                , ID, App, (<$$>), Transaction(..) )
import App.Expression           ( Token(..), Expression )
import Control.Applicative      ( (<$>), (<*>) )
import Control.Monad            ( when )
import Control.Monad.Trans      ( lift )
import Control.Monad.Reader     ( ask, runReaderT, ReaderT )
import Data.Int                 ( Int64 )
import Data.List                ( intercalate )
import Data.Maybe               ( listToMaybe )
import Data.Text                ( pack )
import Data.Textual             ( toLower, trim, replace )
import Data.Time.Extended       ( fromSeconds, toSeconds )
import Database.SQLite.Simple   ( FromRow(..), Query(..), Connection
                                , lastInsertRowId, execute, execute_, field
                                , query, query_, withConnection
                                , withTransaction )
import Foreign.Marshal.Utils    ( toBool, fromBool )

------------------------------------------------------------------------- Types

-- | A database-specific representation of an image.
data DBImage = DBImage ID String Int String String Int Int Integer Integer Int

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

-- | Deletes the image with the given ID.
deleteImage :: ID -> Transaction ()
deleteImage id = do
    conn <- ask
    lift $ execute conn command [id]
    where command = "DELETE FROM post WHERE id = ?"

-- | Inserts a new image into the database and returns its ID.
insertImage :: Image -> Transaction ID
insertImage image = do
    conn <- ask
    
    let (DBImage _ title fav hash ext w h c m size) = fromImage image
    
    lift $ do
        execute conn postCommand (title, fav, c, m)
        postID <- lastInsertRowId conn
        execute conn imageCommand (postID, hash, w, h, size, ext)
        return postID
        
    where
        postCommand  = "INSERT INTO post \
                       \(title, is_favourite, created, modified) \
                       \VALUES (?, ?, ?, ?);"

        imageCommand = "INSERT INTO image \
                       \(post_id, hash, width, height, file_size, extension) \
                       \VALUES (?, ?, ?, ?, ?, ?);"

-- | Returns the image from the database with the given ID. If no image exists,
-- | nothing is returned.
selectImage :: ID -> Transaction (Maybe (Entity Image))
selectImage id = do
    conn <- ask
    lift $ listToMaybe <$> toImage <$$> query conn command [id]
    where command = "SELECT p.id, p.title, p.is_favourite, i.hash, \
                    \i.extension, i.width, i.height, p.created, p.modified, \
                    \i.file_size \
                    \FROM post p \
                    \INNER JOIN image i ON i.post_id = p.id \
                    \WHERE i.id = ?;"

-- | Returns a list of all images from the database.
selectImages :: Transaction [Entity Image]
selectImages = do
    conn <- ask
    lift $ toImage <$$> query_ conn command
    where command = "SELECT p.id, p.title, p.is_favourite, i.hash, \
                    \i.extension, i.width, i.height, p.created, p.modified, \
                    \i.file_size \
                    \FROM post p \
                    \INNER JOIN image i ON i.post_id = p.id \
                    \ORDER BY p.id;"

-- | Returns a list of all images from the database with tags that satisfy the
-- | given expression.
selectImagesByExpression :: Expression -> Transaction [Entity Image]
selectImagesByExpression expr = do
    conn <- ask
    lift $ toImage <$$> query_ conn (Query $ pack command)
    where command = "SELECT DISTINCT p.id, p.title, p.is_favourite, i.hash, \
                    \i.extension, i.width, i.height, p.created, p.modified, \
                    \i.file_size \
                    \FROM post p \
                    \INNER JOIN image i ON i.post_id = p.id "
                    ++ generateWhere expr ++
                    "ORDER BY p.id;"

-- | Returns whether or not an image already exists with the given hash.
selectHashExists :: String -> Transaction Bool
selectHashExists hash = do
    conn <- ask
    [results] <- lift $ query conn command [hash] :: Transaction [Int]
    return (results > 0)
    where command = "SELECT COUNT(*) FROM image WHERE hash = ?"

-- | Updates the image in the database.
updateImage :: Entity Image -> Transaction ()
updateImage (Entity id image) = do
    conn <- ask
    let (DBImage _ title fav _ _ _ _ _ modified size) = fromImage image
    lift $ execute conn command (title, fav, modified, id)
    where command = "UPDATE post SET \
                    \title = ?, \
                    \is_favourite = ?, \
                    \modified = ? \
                    \WHERE id = ?;"

-------------------------------------------------------------------------- Tags

-- | Returns a list of all tags from the database.
selectTags :: Transaction [Entity Tag]
selectTags = do
    conn <- ask
    lift $ toTag <$$> query_ conn command
    where command = "SELECT id, name FROM tag ORDER BY name;"

-- | Returns a list of all tags associated to the image with the given ID.
selectTagsByImage :: ID -> Transaction [Entity Tag]
selectTagsByImage id = do
    conn <- ask
    lift $ toTag <$$> query conn command [id]
    where command = "SELECT t.id, t.name \
                    \FROM tag t \
                    \INNER JOIN post_tag pt ON pt.tag_id = t.id \
                    \WHERE pt.post_id = ? \
                    \ORDER BY name;"

-- | Deletes all tags from the database that are not associated to any image.
cleanTags :: Transaction ()
cleanTags = do
    conn <- ask
    lift $ do
        orphanTags <- query_ conn (Query $ pack command1) :: IO [ID]
        mapM_ (\x -> execute conn command2 [x]) orphanTags
    where
        command1 = "SELECT id FROM tag WHERE NOT EXISTS (" ++ subquery ++ ");"
        subquery = "SELECT * FROM post_tag WHERE tag_id = tag.id"
        command2 = "DELETE FROM tag WHERE id = ?;"

----------------------------------------------------------------- Relationships

-- | Associates the image with the given ID with all tags that map to the given
-- | list of names. If any tag does not eixsts, it is added to the database.
attachTags :: [String] -> ID -> Transaction ()
attachTags tags postID = do
    conn <- ask
    mapM_ (attachTag conn postID) tags
    where 
        attachTag conn postID name = lift $ do
            postExists <- not . null <$> getPost conn
            when postExists $ do
                tagID <- getTagID conn name
                case tagID of
                    Nothing -> insert conn name >>= attach conn postID
                    Just id -> attach conn postID id
        attach c a b = execute c attachCmd (a, b)
        getTagID c a = listToMaybe <$> query c selectCmd [a] :: IO (Maybe ID)
        getPost c    = query c getPstCmd [postID] :: IO [ID]
        insert c a   = execute c insertCmd [a] >> lastInsertRowId c
        selectCmd    = "SELECT id FROM tag WHERE name = ?;"
        insertCmd    = "INSERT INTO tag (name) VALUES (?);"
        attachCmd    = "INSERT INTO post_tag (post_id, tag_id) VALUES (?, ?);"
        getPstCmd    = "SELECT id FROM post WHERE id = ?;"

-- | Disassociates all tags from the image with the given ID.
clearTags :: ID -> Transaction ()
clearTags id = do
    conn <- ask
    lift $ execute conn deleteCmd [id]
    where deleteCmd = "DELETE FROM post_tag WHERE post_id = ?;"

----------------------------------------------------------------------- Utility

-- | Transforms an Image into a DBImage.
fromImage :: Image -> DBImage
fromImage (Image title fav hash ext w h created modified size) =
    (DBImage 0 title (fromBool fav) hash ext w h created' modified' size)
    where
        created'  = toSeconds created
        modified' = toSeconds modified

-- | Transforms a DBImage into an Entity Image.
toImage :: DBImage -> Entity Image
toImage (DBImage id title fav hash ext w h created modified size) =
    Entity id (Image title (toBool fav) hash ext w h created' modified' size)
    where
        created'  = fromSeconds created
        modified' = fromSeconds modified

-- | Transforms a DBTag into an Entity Tag.
toTag :: DBTag -> Entity Tag
toTag (DBTag id name) = Entity id (Tag name)

-- | Converts the given expression to a SQL where clause.
generateWhere :: Expression -> String
generateWhere []   = ""
generateWhere expr = "WHERE " ++ intercalate " AND " (map tagLike expr) ++ " "
    where
        tagLike = \case
            Included x ->     "EXISTS (" ++ findTag (escape x) ++ ")"
            Excluded x -> "NOT EXISTS (" ++ findTag (escape x) ++ ")"
        escape = toLower . trim
                         . replace "%" "\\%" 
                         . replace "_" "\\_" 
                         . replace "'" "''"       
        findTag x = "SELECT 1 \
                    \FROM post_tag pt \
                    \INNER JOIN tag t \
                    \ON pt.tag_id = t.id AND pt.post_id = p.id \
                    \WHERE t.name LIKE '" ++ x ++ "%' ESCAPE '\\'"
