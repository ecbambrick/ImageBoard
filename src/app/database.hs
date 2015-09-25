{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}

module App.Database
    ( deleteImage, insertImage, selectHashExists, selectImage, selectImages
    , selectNextImage, selectPreviousImage, updateImage, insertAlbum
    , selectTags, selectTagsByImage, attachTags, cleanTags ) where

import qualified Database.Engine as SQL
                                
import App.Common           ( Album(..), Image(..), Page(..), Tag(..), (<$$>) )
import App.Expression       ( Token(..), Expression )
import Control.Applicative  ( (<$>), (<*>), pure )
import Control.Monad        ( forM_, void )
import Data.Int             ( Int64 )
import Data.Maybe           ( isJust )
import Data.Textual         ( toLower, trim, replace )
import Data.Time.Extended   ( fromSeconds, toSeconds )
import Database.Engine      ( Entity(..), Transaction(..), ID, FromRow
                            , fromEntity, fromRow, field )
import Database.Query       ( OrderBy(..), Table, Query, (.|), (.&), (~%), (%%)
                            , (.=), (.>), (.<), (*=), (<<), asc, clearOrder
                            , desc, exists, limit, from, nay, offset, on
                            , retrieve, wherever )

------------------------------------------------------------------------- Types

-- | The direction to use when selecting an adjacent entity in the database.
data Direction = Next | Prev

--------------------------------------------------------------------- Instances

instance FromRow Int64 where
    fromRow = field

instance FromRow Int where
    fromRow = field
    
instance FromRow (Entity Tag) where
    fromRow = do
        entity <- Entity <$> field
        tag    <- Tag    <$> field

        return (entity tag)

instance FromRow (Entity Image) where
    fromRow = do
        entity <- Entity <$> field
        image  <- Image  <$> field      <*> bool field <*> field
                         <*> field      <*> field      <*> field
                         <*> time field <*> time field <*> field 
                         <*> pure []
                         
        return (entity image)
        
        where bool = fmap toBool
              time = fmap fromSeconds

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

    case results of
        Nothing    -> return Nothing
        Just image -> Just <$> withTags image

-- | Gets a list of images from the database. If a non-empty expression is
-- | passed in, only images that satisfy the expression will be returned.
selectImages :: Expression -> Int -> Int -> Transaction [Entity Image]
selectImages expression from count = do
    results <- SQL.query $ do
        i <- images
        satisfying expression i
        offset from
        limit count
    
    sequence (withTags <$> results)

-- | Returns the image from the database ordered after the image with the 
-- | given ID. If no image exists, nothing is returned.
selectNextImage :: ID -> Expression -> Transaction (Maybe (Entity Image))
selectNextImage = selectAdjacentImage Next

-- | Returns the image from the database ordered before the image with the 
-- | given ID. If no image exists, nothing is returned.
selectPreviousImage :: ID -> Expression -> Transaction (Maybe (Entity Image))
selectPreviousImage = selectAdjacentImage Prev

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

-------------------------------------------------------------------------- Tags

-- | Returns a list of all tags from the database.
selectTags :: Transaction [Entity Tag]
selectTags = SQL.query tags

-- | Returns a list of all tags attached to the image with the given ID.
selectTagsByImage :: ID -> Transaction [Entity Tag]
selectTagsByImage postID = SQL.query $ do
    t  <- tags
    pt <- from "post_tag" `on` ("tag_id" *= t "id")
    wherever (pt "post_id" .= postID)

-- | Deletes all tags from the database that are not attached to any image.
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

------------------------------------------------------------------------ Albums

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

----------------------------------------------------------------- Relationships

-- | Associates the image with the given ID with the tag with the given name. 
-- | If the tag does not eixst, it is created.
attachTag :: ID -> String -> Transaction ()
attachTag postID tagName = do
    tagID <- SQL.single $ do
        t <- from "tag"
        wherever (t "name" .= tagName)
        retrieve [t "id"]
    
    tagID' <- case tagID of
        Nothing -> SQL.insert "tag" [ "name" << tagName ]
        Just id -> return id
    
    void $ SQL.insert "post_tag"
        [ "post_id" << postID
        , "tag_id"  << tagID' ]

-- | Associates the image with the given ID with all tags that map to the given
-- | list of names. If any tag does not eixsts, it is created.
attachTags :: [String] -> ID -> Transaction ()
attachTags tagNames postID = mapM_ (attachTag postID) tagNames

------------------------------------------------------------------ Query pieces

-- | Selects images from the database.
images :: Query Table
images = do
    p <- from "post"
    i <- from "image" `on` ("post_id" *= p "id")
    desc (p "modified")
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

-- | Adds the list of tag names to the given image entity.
withTags :: Entity Image -> Transaction (Entity Image)
withTags (Entity id image) = do
    tagNames <- tagName . fromEntity <$$> selectTagsByImage id
    return (Entity id image { imageTagNames = tagNames })

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
            ordering (p "modified")
            ordering (p "id")
        
    firstImage <- case modified of
        Nothing -> return Nothing
        Just m  -> SQL.single $ do
            p <- images 
            satisfying expression p
            clearOrder
            ordering (p "modified")
            ordering (p "id")
    
    case (nextImage, firstImage) of
        (Just image, _) -> Just <$> withTags image
        (_, Just image) -> Just <$> withTags image
        (_, _         ) -> return Nothing

----------------------------------------------------------------------- Utility

-- | Converts an integer to a boolean.
toBool :: Int -> Bool
toBool 0 = False
toBool _ = True

-- | Converts a boolean to an integer.
fromBool :: Bool -> Int
fromBool False = 0
fromBool True  = 1
