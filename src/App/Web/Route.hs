{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module App.Web.Route
    ( albumRoute, albumsRoute, imageRoute, imagesRoute, pageRoute, uploadRoute
    , album, albums, image, images, page, upload ) where

import qualified Data.Text   as Text
import qualified Network.URI as URI
import qualified Web.Spock   as Spock

import Data.Monoid     ( (<>) )
import Data.Text       ( Text )
import Database.Engine ( ID )
import Web.Spock       ( (<//>) )

------------------------------------------------------------------------ Routes

-- | The route to an album with the given ID.
albumRoute :: Spock.Path '[ID]
albumRoute = "album" <//> Spock.var

-- | The route to the list of albums.
albumsRoute :: Spock.Path '[]
albumsRoute = "albums"

-- | The route to the image with the given ID.
imageRoute :: Spock.Path '[ID]
imageRoute = "image" <//> Spock.var

-- | The route to the list of images.
imagesRoute :: Spock.Path '[]
imagesRoute = "images"

-- | The route to the given page number from the album with the given ID.
pageRoute :: Spock.Path '[ID, Int]
pageRoute = "album" <//> Spock.var <//> Spock.var

-- | The route to upload a new post.
uploadRoute :: Spock.Path '[]
uploadRoute = "upload"

------------------------------------------------------------- Routing Functions

-- | Returns the route to an album with the given ID and query.
album :: ID -> Text
album id = Spock.renderRoute albumRoute id

-- | Returns the route to the list of albums on the given page with the given
-- | query.
albums :: Int -> String -> Text
albums page query =
    let route  = Spock.renderRoute albumsRoute
        params = [("page", if page <= 1 then "" else show page), ("q", query)]

    in withParameters route params

-- | Returns the route to the image with the given ID filtered by the given
-- | query.
image :: ID -> String -> Text
image id query =
    let route  = Spock.renderRoute imageRoute id
        params = [("q", query)]

    in withParameters route params

-- | Returns the route to the list of images on the given page with the given
-- | query.
images :: Int -> String -> Text
images page query =
    let route  = Spock.renderRoute imagesRoute
        params = [("page", if page <= 1 then "" else show page), ("q", query)]

    in route `withParameters` params

-- | Returns the route to the given page number from the album with the given ID.
page :: ID -> Int -> Text
page = Spock.renderRoute pageRoute

-- | Returns the route to upload a new post.
upload :: Text
upload = Spock.renderRoute uploadRoute

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
