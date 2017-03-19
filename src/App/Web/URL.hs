{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}

module App.Web.URL
    ( image, images, imageFile, imageThumb
    , album, albums, albumThumb
    , page, pageFile, pageThumb
    , tags, upload ) where

import qualified App.Path       as Path
import qualified App.Web.Route  as Route
import qualified Data.Text      as Text
import qualified Network.URI    as URI
import qualified Web.Spock      as Spock

import App.Core.Types  ( Album(..), Image(..), Page(..), Scope(..), ID )
import Data.Monoid     ( (<>) )
import Data.Text       ( Text )
import Data.Textual    ( replace )
import System.FilePath ( (</>), (<.>) )

-------------------------------------------------------------------- Image URLs

-- | Returns the route to the image with the given ID filtered by the given
-- | query and scope.
image :: Scope -> ID -> String -> Text
image scope id query =
    let route     = Spock.renderRoute Route.image (scopeName scope) id
        params    = [("q", query)]

    in withParameters route params

-- | Returns the route to the list of images on the given page with the given
-- | query and scope.
images :: Scope -> Int -> String -> Text
images scope page query =
    let route     = Spock.renderRoute Route.images (scopeName scope)
        params    = [("page", if page <= 1 then "" else show page), ("q", query)]

    in route `withParameters` params

-- | Returns the route for the file of the given image.
imageFile :: Image -> Text
imageFile image = pathToURL (Path.relativeImageFile image)

-- | Returns the route for the thumbnail of the given image.
imageThumb :: Image -> Text
imageThumb image = pathToURL (Path.relativeImageThumb image)

-------------------------------------------------------------------- Album URLs

-- | Returns the route to an album with the given ID, query, and scope.
album :: Scope -> ID -> Text
album scope id = Spock.renderRoute Route.album (scopeName scope) id

-- | Returns the route to the list of albums on the given page with the given
-- | query and scope.
albums :: Scope -> Int -> String -> Text
albums scope page query =
    let route     = Spock.renderRoute Route.albums (scopeName scope)
        params    = [("page", if page <= 1 then "" else show page), ("q", query)]

    in withParameters route params

-- | Returns the route for the thumbnail of the given album.
albumThumb :: Album -> Text
albumThumb Album {..} = pathToURL (Path.relativeAlbumDirectory albumID </> "thumbnail.jpg")

--------------------------------------------------------------------- Page URLs

-- | Returns the route to the given page number from the album with the given
-- | ID and scope.
page :: Scope -> ID -> Int -> Text
page scope = Spock.renderRoute Route.page (scopeName scope)

-- | Returns the route for the file for the given page of the album with the
-- | given ID.
pageFile :: ID -> Page -> Text
pageFile id Page {..} = pathToURL path
    where path = Path.relativeAlbumDirectory id </> show pageNumber <.> pageExtension

-- | Returns the route for the thumbnail for the given page of the album with
-- | the given ID.
pageThumb :: ID -> Page -> Text
pageThumb id Page {..} = pathToURL path
    where path = Path.relativeAlbumDirectory id </> "t" ++ show pageNumber <.> "jpg"

-------------------------------------------------------------------- Misc. URLs

-- | Returns the route to the list of tags with the given scope.
tags :: Scope -> String -> Text
tags scope query =
    let route  = Spock.renderRoute Route.tags (scopeName scope)
        params = [("q", query)]

    in route `withParameters` params

-- | Returns the route to upload a new post.
upload :: Scope -> Text
upload scope = Spock.renderRoute Route.upload (scopeName scope)

----------------------------------------------------------------------- Utility

-- | Adds the given list of parameter key-value pairs to the given URL.
withParameters :: Text -> [(String, String)] -> Text
withParameters url params =
    let escape             = Text.pack . URI.escapeURIString URI.isUnreserved
        encodePair ("", _) = ""
        encodePair (_, "") = ""
        encodePair (k,  v) = escape k <> "=" <> escape v
        encodedParams      = filter (not . Text.null) $ map encodePair params
        results            = Text.intercalate "&" encodedParams

    in if Text.null results
        then url
        else url <> "?" <> results

-- | Converts the given file path to a URL.
pathToURL :: FilePath -> Text
pathToURL path = Text.pack $ replace "\\" "/" ("/" ++ path)
