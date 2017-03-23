{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}

module App.Database
    ( createDatabase, deleteDatabase

    , deletePost, markPostAsDeleted

    , insertImage, selectHashExists, selectImagesCount, selectImage
    , selectImages, selectNextImage, selectPreviousImage
    , selectRandomImage, selectRandomImages, updateImage

    , insertAlbum, selectAlbum, selectAlbums
    , selectAlbumsCount, updateAlbum

    , deleteScope, insertScope, selectScope, selectScopeID, updateScope

    , selectTagIDByName, selectTagDetails, selectTagCategories
    , attachTags, cleanTags, detachTags, attachCategories
    , selectCategoryIDByName
    ) where

import qualified Database.Engine  as SQL
import qualified Data.DateTime    as DateTime
import qualified Data.Text        as Text
import qualified Data.Traversable as Traversable

import App.Config            ( Config(..) )
import App.Core.Types        ( Album(..), Image(..), Page(..), Scope(..)
                             , SimpleTag(..), App, ID )
import App.Expression        ( Match(..), Token(..), Expression )
import Control.Applicative   ( (<|>) )
import Control.Monad.Reader  ( asks, liftIO, forM_, void, unless )
import Data.DateTime         ( DateTime )
import Data.Functor.Extended ( (<$$>) )
import Data.Int              ( Int64 )
import Data.Maybe            ( isJust, listToMaybe, fromMaybe )
import Data.Text             ( Text )
import Data.Textual          ( replace, splitOn, toLower, trim )
import Database.Engine       ( Transaction(..), FromRow, fromRow, field )
import Database.Query        ( OrderBy(..), Table, Query, (.|), (.&), (~%), (%%)
                             , (.=), (./=), (.>), (.>=), (.<), (*=), (<<), asc
                             , clearOrder, count, desc, exists, groupBy, limit
                             , from, fromLeft, nay, offset, on, randomOrder
                             , retrieve, wherever )
import System.Directory      ( doesFileExist, removeFile )

------------------------------------------------------------------------- Types

-- | The direction to use when selecting an adjacent entity in the database.
data Direction = Next | Prev

-- | Wrapper around string to create a FromRow instance.
data WrappedString = WrappedString { innerString :: String }

instance FromRow Int64 where
    fromRow = field

instance FromRow Int where
    fromRow = field

instance FromRow WrappedString where
    fromRow = WrappedString <$> field

instance FromRow Album where
    fromRow = Album <$> field      <*> field <*> bool field <*> time field
                    <*> time field <*> field <*> pure []    <*> pure []

instance FromRow Image where
    fromRow = Image  <$> field      <*> field <*> bool field <*> field
                     <*> field      <*> field <*> field      <*> time field
                     <*> time field <*> field <*> pure []

instance FromRow Page where
    fromRow = Page <$> field <*> field <*> field

instance FromRow SimpleTag where
    fromRow = SimpleTag <$> field <*> field <*> time field

instance FromRow Scope where
    fromRow = Scope <$> field <*> field

---------------------------------------------------------------------- Database

-- | Creates a new database using the configuration's database connection
-- | string. If a database already exists, this function will do nothing.
createDatabase :: App ()
createDatabase = do
    database <- asks configDatabaseConnection
    exists   <- liftIO $ doesFileExist database
    schema   <- liftIO $ readFile "schema.sql"

    liftIO $ unless exists $ do
        SQL.runDatabase database $ do
            let commands = filter (/= "\n") $ splitOn ";" schema
            mapM_ SQL.execute commands

-- | Deletes the database specified in the configuration's database connection
-- | string. If the database does not exist, this function will do nothing.
deleteDatabase :: App ()
deleteDatabase = do
    database <- asks configDatabaseConnection

    liftIO $ removeFile database

------------------------------------------------------------------------- Posts

-- | Marks the post with the given ID as deleted.
markPostAsDeleted :: ID -> Transaction ()
markPostAsDeleted id = SQL.update "post" ("id" *= id) [ "is_deleted" << "1" ]

-- | Deletes the post with the given ID.
deletePost :: ID -> Transaction ()
deletePost id = SQL.delete "post" ("id" *= id)

------------------------------------------------------------------------ Images

-- | Inserts a new image into the database and returns its ID.
insertImage :: Image -> Transaction ID
insertImage Image {..} = do
    postID <- SQL.insert "post"
        [ "title"        << imageTitle
        , "created"      << DateTime.toSeconds imageCreated
        , "modified"     << DateTime.toSeconds imageModified
        , "is_favourite" << fromBool imageIsFavourite
        , "is_deleted"   << fromBool False ]

    SQL.insert "image"
        [ "post_id"      << postID
        , "hash"         << imageHash
        , "width"        << imageWidth
        , "height"       << imageHeight
        , "file_size"    << imageFileSize
        , "extension"    << imageExtension
        , "is_animated"  << fromBool False ]

    return postID

-- | Returns the image from the database with the given ID. If no image exists,
-- | nothing is returned.
selectImage :: ID -> Transaction (Maybe Image)
selectImage id = do
    results <- SQL.single (images >>= wherever . ("id" *= id))

    Traversable.sequence (withImageTags <$> results)

-- | Gets a list of images from the database. If a non-empty expression is
-- | passed in, only images that satisfy the expression will be returned.
selectImages :: Expression -> Int -> Int -> Transaction [Image]
selectImages expression from count = do
    now     <- liftIO $ DateTime.getCurrentTime
    results <- SQL.query (images >>= paginated expression now from count)

    Traversable.sequence (withImageTags <$> results)

-- | Returns the image from the database ordered after the image with the
-- | given ID. If no image exists, nothing is returned.
selectNextImage :: ID -> Expression -> Transaction (Maybe Image)
selectNextImage = selectAdjacentImage Next

-- | Returns the image from the database ordered before the image with the
-- | given ID. If no image exists, nothing is returned.
selectPreviousImage :: ID -> Expression -> Transaction (Maybe Image)
selectPreviousImage = selectAdjacentImage Prev

-- | Returns a random image from the database that satisfies the given
-- | expression. If no image exists, nothing is returned.
selectRandomImage :: Expression -> Transaction (Maybe Image)
selectRandomImage expression = listToMaybe <$> selectRandomImages expression 1

-- | Returns random images from the database that satisfy the given
-- | expression.
selectRandomImages :: Expression -> Int -> Transaction [Image]
selectRandomImages expression count = do
    now     <- liftIO $ DateTime.getCurrentTime
    results <- SQL.query $ do
        i <- images
        paginated expression now 0 count i
        randomOrder

    Traversable.sequence (withImageTags <$> results)

-- | Updates the image in the database.
updateImage :: Image -> Transaction ()
updateImage Image {..} = SQL.update "post" ("id" *= imageID)
    [ "title"        << imageTitle
    , "is_favourite" << imageIsFavourite
    , "modified"     << DateTime.toSeconds imageModified ]

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

-- | Returns a list of all tags attached to the post with the given ID.
selectTagsByPost :: ID -> Transaction [String]
selectTagsByPost postID = do
    results <- SQL.query $ do
        t  <- tags
        pt <- from "post_tag" `on` ("tag_id" *= t "id")
        wherever (pt "post_id" .= postID)
        retrieve [t "name"]

    return (innerString <$> results)

-- | Returns tag data for tags that are attached to at least one post that
-- | satisfies the given expression.
-- |
-- | The results include the following data:
-- |   * the tag ID
-- |   * the tag name
-- |   * the ID of a post with the tag attached
-- |   * the number of images with the tag
-- |   * the number of albums with the tag
selectTagDetails :: Expression -> Transaction [(ID, String, DateTime, ID, Int, Int)]
selectTagDetails expression =  do
    now <- liftIO $ DateTime.getCurrentTime
    SQL.query $ do
        t  <- from     "tag"
        pt <- from     "post_tag" `on` ("tag_id"  *= t  "id")
        p  <- from     "post"     `on` ("id"      *= pt "post_id")
        i  <- fromLeft "image"    `on` ("post_id" *= p  "id")
        a  <- fromLeft "album"    `on` ("post_id" *= p  "id")
        satisfying expression now p
        wherever (p "is_deleted" .= False)
        groupBy  (t "name")
        asc      (t "name")
        retrieve [t "id", t "name", t "created", p "id", count (i "id"), count (a "id")]

-- | Returns the list of category names for the tag with the given ID.
selectTagCategories :: ID -> Transaction [String]
selectTagCategories tagID = do
    categories <- SQL.query $ do
        c  <- from "category"
        tc <- from "tag_category" `on` ("category_id" *= c "id")
        wherever (tc "tag_id" .= tagID)
        retrieve [c "name"]

    return (innerString <$> categories)

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
attachTag :: ID -> DateTime -> String -> Transaction ()
attachTag postID created tagName = do
    let existingID = selectTagIDByName tagName
        newID      = SQL.insert "tag"
                        [ "name"    << tagName
                        , "created" << DateTime.toSeconds created ]

    tagID <- fromMaybe <$> newID <*> existingID

    void $ SQL.insert "post_tag"
        [ "post_id" << postID
        , "tag_id"  << tagID ]

-- | Associates the post with the given ID with all tags that map to the given
-- | list of names. If any tag does not exists, it is created.
attachTags :: [String] -> ID -> Transaction ()
attachTags tagNames postID = do
    now <- liftIO $ DateTime.getCurrentTime
    mapM_ (attachTag postID now) tagNames

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

-- | Associates the tag with the given ID with all the categories with the given
-- | IDs.
attachCategories :: ID -> [ID] -> Transaction ()
attachCategories tagID categoryIDs =
    forM_ categoryIDs $ \categoryID ->
        SQL.insert "tag_category"
            [ "tag_id"      << tagID
            , "category_id" << categoryID ]

-- | Returns the ID of the tag with the given name or nothing if the tag name
-- | does not exist.
selectTagIDByName :: String -> Transaction (Maybe ID)
selectTagIDByName tagName = SQL.single $ do
    t <- from "tag"
    wherever (t "name" .= tagName)
    retrieve [t "id"]

-- | Returns the ID of the category with the given name or nothing if the
-- | category name does not exist.
selectCategoryIDByName :: String -> Transaction (Maybe ID)
selectCategoryIDByName categoryName = SQL.single $ do
    t <- from "category"
    wherever (t "name" .= categoryName)
    retrieve [t "id"]

------------------------------------------------------------------------ Albums

-- | Inserts a new album into the database and returns its ID.
insertAlbum :: Album -> Transaction ID
insertAlbum Album {..} = do
    postID <- SQL.insert "post"
        [ "title"        << albumTitle
        , "created"      << DateTime.toSeconds albumCreated
        , "modified"     << DateTime.toSeconds albumModified
        , "is_favourite" << fromBool albumIsFavourite
        , "is_deleted"   << fromBool False ]

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
selectAlbum :: ID -> Transaction (Maybe Album)
selectAlbum id = do
    results          <- SQL.single (albums >>= wherever . ("id" *= id))
    withPages        <- Traversable.sequence (withPages     <$> results)
    withPagesAndTags <- Traversable.sequence (withAlbumTags <$> withPages)

    return withPagesAndTags

-- | Gets a list of albums from the database. If a non-empty expression is
-- | passed in, only albums that satisfy the expression will be returned.
selectAlbums :: Expression -> Int -> Int -> Transaction [Album]
selectAlbums expression from count = do
    now              <- liftIO $ DateTime.getCurrentTime
    results          <- SQL.query (albums >>= paginated expression now from count)
    withPages        <- Traversable.sequence (withPages <$> results)
    withPagesAndTags <- Traversable.sequence (withAlbumTags <$> withPages)

    return withPagesAndTags

-- | Returns the total number of albums that satisfy the given expression.
selectAlbumsCount :: Expression -> Transaction Int
selectAlbumsCount = selectCount "album"

-- | Updates the album in the database.
updateAlbum :: Album -> Transaction ()
updateAlbum Album {..} = SQL.update "post" ("id" *= albumID)
    [ "title"        << albumTitle
    , "is_favourite" << albumIsFavourite
    , "modified"     << DateTime.toSeconds albumModified ]

------------------------------------------------------------------------- Pages

-- | Returns the list of all pages owned by the album with the given ID.
selectPagesByAlbum :: ID -> Transaction [Page]
selectPagesByAlbum postID = SQL.query $ do
    a <- from "album"
    p <- from "page" `on` ("album_id" *= a "id")
    wherever (a "post_id" .= postID)
    retrieve [ p "number", p "title", p "extension" ]
    asc (p "number")

------------------------------------------------------------------------ Scopes

-- | Deletes the scope with the given name.
deleteScope :: String -> Transaction ()
deleteScope name = SQL.delete "scope" ("name" *= name)

-- | Inserts a new scope into the database.
insertScope :: Scope -> Transaction ID
insertScope Scope {..} = SQL.insert "scope"
    [ "name"       << scopeName
    , "expression" << scopeExpression ]

-- | Returns the scope with the given name from the database  If no scope
-- | exists, nothing is returned.
selectScope :: String -> Transaction (Maybe Scope)
selectScope name = SQL.single $ do
    s <- from "scope"
    wherever (s "name" .= name)
    retrieve [s "name", s "expression"]

-- | Returns the ID of the scope with the given name from the database. If no
-- | scope exists, nothing is returned.
selectScopeID :: String -> Transaction (Maybe ID)
selectScopeID name = SQL.single $ do
    s <- from "scope"
    wherever (s "name" .= name)
    retrieve [s "id"]

-- | Updates the scope in the database.
updateScope :: ID -> Scope -> Transaction ()
updateScope id Scope {..} = SQL.update "scope" ("id" *= id)
    [ "name"       << scopeName
    , "expression" << scopeExpression ]

---------------------------------------------------------------- Query segments

-- | Selects albums from the database.
albums :: Query Table
albums = do
    p  <- from "post"
    a  <- from "album" `on` ("post_id" *= p "id")
    desc (p "created")
    desc (p "id")
    wherever (p "is_deleted" .= False)
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
    wherever (p "is_deleted" .= False)
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
satisfying :: Expression -> DateTime -> Table -> Query ()
satisfying [] _ _           = return ()
satisfying expression now p = forM_ expression $ \case
    Included (MatchTags  x) -> wherever $       exists $ query x
    Excluded (MatchTags  x) -> wherever $ nay . exists $ query x
    Included (MatchID    x) -> wherever (p "id" .=  x)
    Excluded (MatchID    x) -> wherever (p "id" ./= x)
    Excluded MatchToday     -> wherever (p "created" .<  today)
    Included MatchToday     -> wherever (p "created" .>= today)
    Excluded MatchThisWeek  -> wherever (p "created" .<  thisWeek)
    Included MatchThisWeek  -> wherever (p "created" .>= thisWeek)
    Excluded MatchThisMonth -> wherever (p "created" .<  thisMonth)
    Included MatchThisMonth -> wherever (p "created" .>= thisMonth)

    where
        today     = DateTime.toSeconds $ DateTime.dropTime $ now
        thisWeek  = DateTime.toSeconds $ DateTime.dropTime $ DateTime.addDays now (-6)
        thisMonth = DateTime.toSeconds $ DateTime.dropTime $ DateTime.addDays now (-29)

        query x = do
            let x' = toLower (trim x)
            pt <- from "post_tag"
            t  <- from "tag" `on` \t -> t "id" .= pt "tag_id"
                                     .& p "id" .= pt "post_id"
            wherever (t "name" ~% x' .| t "name" %% (' ':x'))

-- | Returns a paginated query for the given table using the given expression,
-- | offset, and limit.
paginated :: Expression -> DateTime -> Int -> Int -> Table -> Query ()
paginated expression now from count table = do
    satisfying expression now table
    offset from
    limit count

--------------------------------------------------------------------- Modifiers

-- | Adds the list of pages to the given album entity.
withPages :: Album -> Transaction Album
withPages album @ Album {..} = do
    pages <- selectPagesByAlbum albumID
    return album { albumPages = pages }

-- | Adds the list of tag names to the given image entity.
withImageTags :: Image -> Transaction Image
withImageTags image @ Image {..} = do
    tagNames <- selectTagsByPost imageID
    return image { imageTagNames = tagNames }

-- | Adds the list of tag names to the given image entity.
withAlbumTags :: Album -> Transaction Album
withAlbumTags album @ Album {..} = do
    tagNames <- selectTagsByPost albumID
    return album { albumTagNames = tagNames }

----------------------------------------------------------------------- Utility

-- | Selects the image adjacent to the image with the given ID in the given
-- | direction. If there is no next/previous image, then the very first or last
-- | image is returned instead. If the image with the given ID does not exist,
-- | nothing is returned.
selectAdjacentImage :: Direction -> ID -> Expression -> Transaction (Maybe Image)
selectAdjacentImage dir id expression = do
    let (operator, ordering) = case dir of
            Next -> ((.<), desc)
            Prev -> ((.>), asc )

    now <- liftIO $ DateTime.getCurrentTime

    modified <- SQL.single $ do
        p <- from "post"
        wherever (p "id" .= id)
        retrieve [p "id"]
        :: Transaction (Maybe Int)

    nextImage <- case modified of
        Nothing -> return Nothing
        Just m  -> SQL.single $ do
            p <- images
            satisfying expression now p
            clearOrder
            wherever (p "id" `operator` m)
            ordering (p "created")
            ordering (p "id")

    firstImage <- case modified of
        Nothing -> return Nothing
        Just m  -> SQL.single $ do
            p <- images
            satisfying expression now p
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
    now <- liftIO $ DateTime.getCurrentTime

    (Just results) <- SQL.single $ do
        p <- from "post"
        t <- from table `on` ("post_id" *= p "id")
        wherever (p "is_deleted" .= False)
        satisfying expression now p
        retrieve [count (p "id")]

    return results

-- | Maps the given integer to a boolean.
bool :: (Functor f) => f Int -> f Bool
bool = fmap (\x -> if x == 0 then False else True)

-- | Maps the given integer to a UTC time.
time :: (Functor f) => f Integer -> f DateTime
time = fmap DateTime.fromSeconds

-- | Converts a boolean to an integer.
fromBool :: Bool -> Int
fromBool False = 0
fromBool True  = 1
