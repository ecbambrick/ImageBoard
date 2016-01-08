{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}

module App.Database
    ( deleteImage, insertImage, selectHashExists, selectImagesCount
    , selectImage, selectImages, selectNextImage, selectPreviousImage
    , selectRandomImage, updateImage, deleteAlbum, insertAlbum, selectAlbum
    , selectAlbums, selectAlbumsCount, updateAlbum, selectTags, attachTags
    , cleanTags, detachTags ) where

import qualified Database.Engine as SQL
import qualified Data.Traversable as Traversable
                                
import App.Common           ( Album(..), Image(..), Page(..), Tag(..), (<$$>) )
import App.Expression       ( Token(..), Expression )
import Control.Applicative  ( (<$>), (<*>), pure )
import Control.Monad        ( forM_, void )
import Data.Int             ( Int64 )
import Data.Maybe           ( isJust )
import Data.Textual         ( toLower, trim, replace )
import Data.DateTime        ( DateTime, fromSeconds, toSeconds )
import Database.Engine      ( Entity(..), Transaction(..), ID, FromRow
                            , fromEntity, fromRow, field )
import Database.Query       ( OrderBy(..), Table, Query, (.|), (.&), (~%), (%%)
                            , (.=), (.>), (.<), (*=), (<<), asc, clearOrder
                            , count, desc, exists, limit, from, nay, offset, on
                            , randomOrder, retrieve, wherever )

------------------------------------------------------------------------- Types

-- | The direction to use when selecting an adjacent entity in the database.
data Direction = Next | Prev

instance FromRow Int64 where
    fromRow = field

instance FromRow Int where
    fromRow = field

instance FromRow (Entity Album) where
    fromRow = do
        entity <- Entity <$> field
        album  <- Album  <$> field      <*> bool field <*> time field
                         <*> time field <*> field      <*> pure []
                         <*> pure []
                         
        return (entity album)

instance FromRow (Entity Image) where
    fromRow = do
        entity <- Entity <$> field
        image  <- Image  <$> field      <*> bool field <*> field
                         <*> field      <*> field      <*> field
                         <*> time field <*> time field <*> field 
                         <*> pure []
                         
        return (entity image)

instance FromRow (Entity Page) where
    fromRow = do
        entity <- Entity <$> field
        page   <- Page   <$> field <*> field <*> field
                         
        return (entity page)
    
instance FromRow (Entity Tag) where
    fromRow = do
        entity <- Entity <$> field
        tag    <- Tag    <$> field

        return (entity tag)

------------------------------------------------------------------------ Images

-- | Deletes the image with the given ID.
deleteImage :: ID -> Transaction ()
deleteImage id = SQL.delete "post" ("id" *= id)

-- | Inserts a new image into the database and returns its ID.
insertImage :: Image -> Transaction ID
insertImage Image {..} = do
    postID <- SQL.insert "post"
        [ "title"        << imageTitle
        , "created"      << toSeconds imageCreated
        , "modified"     << toSeconds imageModified
        , "is_favourite" << fromBool imageIsFavourite ]
        
    SQL.insert "image"
        [ "post_id"      << postID
        , "hash"         << imageHash
        , "width"        << imageWidth
        , "height"       << imageHeight
        , "file_size"    << imageFileSize
        , "extension"    << imageExtension ]

    return postID

-- | Returns the image from the database with the given ID. If no image exists,
-- | nothing is returned.
selectImage :: ID -> Transaction (Maybe (Entity Image))
selectImage id = do
    results <- SQL.single (images >>= wherever . ("id" *= id))
    
    Traversable.sequence (withImageTags <$> results)

-- | Gets a list of images from the database. If a non-empty expression is
-- | passed in, only images that satisfy the expression will be returned.
selectImages :: Expression -> Int -> Int -> Transaction [Entity Image]
selectImages expression from count = do
    results <- SQL.query (images >>= paginated expression from count)
    
    Traversable.sequence (withImageTags <$> results)

-- | Returns the image from the database ordered after the image with the 
-- | given ID. If no image exists, nothing is returned.
selectNextImage :: ID -> Expression -> Transaction (Maybe (Entity Image))
selectNextImage = selectAdjacentImage Next

-- | Returns the image from the database ordered before the image with the 
-- | given ID. If no image exists, nothing is returned.
selectPreviousImage :: ID -> Expression -> Transaction (Maybe (Entity Image))
selectPreviousImage = selectAdjacentImage Prev

-- | Returns a random image from the database that satisfies the given 
-- | expression. If no image exists, nothing is returned.
selectRandomImage :: Expression -> Transaction (Maybe (Entity Image))
selectRandomImage expression = do
    results <- SQL.single $ do
        i <- images 
        satisfying expression i
        randomOrder
        limit 1
    
    Traversable.sequence (withImageTags <$> results)

-- | Updates the image in the database.
updateImage :: Entity Image -> Transaction ()
updateImage (Entity id (Image {..})) = SQL.update "post" ("id" *= id)
    [ "title"        << imageTitle
    , "is_favourite" << imageIsFavourite
    , "modified"     << toSeconds imageModified ]

-- | Returns whether or not an image already exists with the given hash.
selectHashExists :: String -> Transaction Bool
selectHashExists hash = do
    results <- SQL.single $ do
        i <- from "image"
        wherever (i "hash" .= hash)
        retrieve [i "id"]
        :: Transaction (Maybe Int)
    
    return (isJust results)

-- | Returns the total number of images that satisfy the given expression.
selectImagesCount :: Expression -> Transaction Int
selectImagesCount = selectCount "image"

-------------------------------------------------------------------------- Tags

-- | Returns a list of all tags from the database.
selectTags :: Transaction [Entity Tag]
selectTags = SQL.query tags

-- | Returns a list of all tags attached to the post with the given ID.
selectTagsByPost :: ID -> Transaction [Entity Tag]
selectTagsByPost postID = SQL.query $ do
    t  <- tags
    pt <- from "post_tag" `on` ("tag_id" *= t "id")
    wherever (pt "post_id" .= postID)

-- | Deletes all tags from the database that are not attached to any post.
cleanTags :: Transaction ()
cleanTags = do
    orphanTags <- SQL.query $ do
        t <- from "tag"
        retrieve [ t "id" ]
        wherever $ nay $ exists $ do
            pt <- from "post_tag"
            wherever (pt "tag_id" .= t "id") 
        :: Transaction [ID]
    
    forM_ orphanTags $ \id -> 
        SQL.delete "tag" ("id" *= id)

-- | Associates the post with the given ID with the tag with the given name. 
-- | If the tag does not exist, it is created.
attachTag :: ID -> String -> Transaction ()
attachTag postID tagName = do
    tagID  <- selectTagIDByName tagName
    tagID' <- case tagID of
        Nothing -> SQL.insert "tag" [ "name" << tagName ]
        Just id -> return id
    
    void $ SQL.insert "post_tag"
        [ "post_id" << postID
        , "tag_id"  << tagID' ]

-- | Associates the post with the given ID with all tags that map to the given
-- | list of names. If any tag does not exists, it is created.
attachTags :: [String] -> ID -> Transaction ()
attachTags tagNames postID = mapM_ (attachTag postID) tagNames

-- | Disassociates the post with the given ID with the tag with the given name. 
-- | If the tag is not already attached, it is ignored.
detachTag :: ID -> String -> Transaction ()
detachTag postID tagName = do
    tagID <- selectTagIDByName tagName
    
    case tagID of
        Nothing -> return ()
        Just id -> SQL.delete "post_tag" $ \pt -> pt "tag_id"  .= id
                                               .& pt "post_id" .= postID

-- | Disassociates the post with the given ID with all tags that map to the 
-- | given list of names. If any tag is not already attached, it is ignored.
detachTags :: [String] -> ID -> Transaction ()
detachTags tagNames postID = mapM_ (detachTag postID) tagNames

------------------------------------------------------------------------ Albums

-- | Deletes the album with the given ID.
deleteAlbum :: ID -> Transaction ()
deleteAlbum id = SQL.delete "post" ("id" *= id)

-- | Inserts a new album into the database and returns its ID.
insertAlbum :: Album -> Transaction ID
insertAlbum Album {..} = do
    postID <- SQL.insert "post"
        [ "title"        << albumTitle
        , "created"      << toSeconds albumCreated
        , "modified"     << toSeconds albumModified
        , "is_favourite" << fromBool albumIsFavourite ]
        
    albumID <- SQL.insert "album"
        [ "post_id"      << postID
        , "file_size"    << albumFileSize ]
    
    forM_ albumPages $ \Page {..} -> SQL.insert "page"
        [ "album_id"     << albumID
        , "title"        << pageTitle
        , "number"       << pageNumber
        , "extension"    << pageExtension ]

    return postID

-- | Returns the album from the database with the given ID. If no album exists,
-- | nothing is returned.
selectAlbum :: ID -> Transaction (Maybe (Entity Album))
selectAlbum id = do
    results          <- SQL.single (albums >>= wherever . ("id" *= id))
    withPages        <- Traversable.sequence (withPages     <$> results)
    withPagesAndTags <- Traversable.sequence (withAlbumTags <$> withPages)
    
    return withPagesAndTags

-- | Gets a list of albums from the database. If a non-empty expression is
-- | passed in, only albums that satisfy the expression will be returned.
selectAlbums :: Expression -> Int -> Int -> Transaction [Entity Album]
selectAlbums expression from count = do
    results          <- SQL.query (albums >>= paginated expression from count)
    withPages        <- Traversable.sequence (withPages <$> results)
    withPagesAndTags <- Traversable.sequence (withAlbumTags <$> withPages)
    
    return withPagesAndTags

-- | Returns the total number of albums that satisfy the given expression.
selectAlbumsCount :: Expression -> Transaction Int
selectAlbumsCount = selectCount "album"

-- | Updates the album in the database.
updateAlbum :: Entity Album -> Transaction ()
updateAlbum (Entity id (Album {..})) = SQL.update "post" ("id" *= id)
    [ "title"        << albumTitle
    , "is_favourite" << albumIsFavourite
    , "modified"     << toSeconds albumModified ]
    
------------------------------------------------------------------------- Pages

-- | Returns the list of all pages owned by the album with the given ID.
selectPagesByAlbum :: ID -> Transaction [Entity Page]
selectPagesByAlbum postID = SQL.query $ do
    a <- from "album"
    p <- from "page" `on` ("album_id" *= a "id")
    wherever (a "post_id" .= postID)
    retrieve [ p "id", p "title", p "number", p "extension" ]
    asc (p "number")

---------------------------------------------------------------- Query segments

-- | Selects albums from the database.
albums :: Query Table
albums = do
    p  <- from "post"
    a  <- from "album" `on` ("post_id" *= p "id")
    desc (p "created")
    desc (p "id")
    retrieve [ p "id", p "title", p "is_favourite", p "created", p "modified"
             , a "file_size" ]
    return p

-- | Selects images from the database.
images :: Query Table
images = do
    p <- from "post"
    i <- from "image" `on` ("post_id" *= p "id")
    desc (p "created")
    desc (p "id")
    retrieve [ p "id", p "title", p "is_favourite", i "hash", i "extension"
             , i "width", i "height", p "created", p "modified", i "file_size" ]
    return p

-- | Selects tags from the database.
tags :: Query Table
tags = do
    t <- from "tag"
    asc (t "name")
    retrieve [t "id", t "name"]
    return t

-- | Returns a query filter that limits the results such that only results that
-- | satisfy the given expression are returned.
satisfying :: Expression -> Table -> Query ()
satisfying [] _         = return ()
satisfying expression p = forM_ expression $ \case
    Included token -> wherever $       exists $ query (escape token)
    Excluded token -> wherever $ nay . exists $ query (escape token)
    
    where escape  = toLower . trim
          query x = do
              pt <- from "post_tag"
              t  <- from "tag" `on` \t -> t "id" .= pt "tag_id" 
                                       .& p "id" .= pt "post_id"
              wherever (t "name" ~% x .| t "name" %% (' ':x))

-- | Returns a paginated query for the given table using the given expression,
-- | offset, and limit.
paginated :: Expression -> Int -> Int -> Table -> Query ()
paginated expression from count table = do 
    satisfying expression table
    offset from
    limit count

--------------------------------------------------------------------- Modifiers

-- | Adds the list of pages to the given album entity.
withPages :: Entity Album -> Transaction (Entity Album)
withPages (Entity id album) = do
    pages <- fromEntity <$$> selectPagesByAlbum id
    return (Entity id album { albumPages = pages })

-- | Adds the list of tag names to the given image entity.
withImageTags :: Entity Image -> Transaction (Entity Image)
withImageTags (Entity id image) = do
    tagNames <- tagName . fromEntity <$$> selectTagsByPost id
    return (Entity id image { imageTagNames = tagNames })

-- | Adds the list of tag names to the given image entity.
withAlbumTags :: Entity Album -> Transaction (Entity Album)
withAlbumTags (Entity id album) = do
    tagNames <- tagName . fromEntity <$$> selectTagsByPost id
    return (Entity id album { albumTagNames = tagNames })

----------------------------------------------------------------------- Utility

-- | Selects the image adjacent to the image with the given ID in the given 
-- | direction. If there is no next/previous image, then the very first or last
-- | image is returned instead. If the image with the given ID does not exist,
-- | nothing is returned.
selectAdjacentImage :: Direction -> ID -> Expression -> Transaction (Maybe (Entity Image))
selectAdjacentImage dir id expression = do
    let (operator, ordering) = case dir of 
            Next -> ((.<), desc)
            Prev -> ((.>), asc )
                    
    modified <- SQL.single $ do
        p <- from "post"
        wherever (p "id" .= id)
        retrieve [p "id"] 
        :: Transaction (Maybe Int)
        
    nextImage <- case modified of
        Nothing -> return Nothing
        Just m  -> SQL.single $ do
            p <- images
            satisfying expression p
            clearOrder
            wherever (p "id" `operator` m)
            ordering (p "created")
            ordering (p "id")
        
    firstImage <- case modified of
        Nothing -> return Nothing
        Just m  -> SQL.single $ do
            p <- images 
            satisfying expression p
            clearOrder
            ordering (p "created")
            ordering (p "id")
    
    case (nextImage, firstImage) of
        (Just image, _) -> Just <$> withImageTags image
        (_, Just image) -> Just <$> withImageTags image
        (_, _         ) -> return Nothing

-- | Returns the total number of rows from the table with the given name that
-- | satisfy the given expression.
selectCount :: String -> Expression -> Transaction Int
selectCount table expression = do
    results <- SQL.query $ do
        p <- from "post"
        t <- from table `on` ("post_id" *= p "id")
        satisfying expression p
        retrieve [count]
        
    return (head results)

-- | Returns the ID of the tag with the given name or nothing if the tag name
-- | does not exist.
selectTagIDByName :: String -> Transaction (Maybe ID)
selectTagIDByName tagName = SQL.single $ do
    t <- from "tag"
    wherever (t "name" .= tagName)
    retrieve [t "id"]

-- | Maps the given integer to a boolean.
bool :: (Functor f) => f Int -> f Bool
bool = fmap (\x -> if x == 0 then False else True)

-- | Maps the given integer to a UTC time.
time :: (Functor f) => f Integer -> f DateTime
time = fmap fromSeconds

-- | Converts a boolean to an integer.
fromBool :: Bool -> Int
fromBool False = 0
fromBool True  = 1
