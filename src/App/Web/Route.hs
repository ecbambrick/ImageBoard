{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module App.Web.Route
    ( albumRoute, albumsRoute, imageRoute, imagesRoute, pageRoute, uploadRoute
    , album, albums, image, images, page, upload ) where

import qualified App.Core.Scope as Scope
import qualified Data.Text      as Text
import qualified Network.URI    as URI
import qualified Web.Spock      as Spock

import App.Core.Types  ( Scope(..) )
import Data.Monoid     ( (<>) )
import Data.Text       ( Text )
import Database.Engine ( ID )
import Web.Spock       ( (<//>) )

------------------------------------------------------------------------ Routes

-- | The route to an album with the given ID.
albumRoute :: Spock.Path '[String, ID]
albumRoute = Spock.var <//> "album" <//> Spock.var

-- | The route to the list of albums.
albumsRoute :: Spock.Path '[String]
albumsRoute = Spock.var <//> "albums"

-- | The route to the image with the given ID.
imageRoute :: Spock.Path '[String, ID]
imageRoute = Spock.var <//> "image" <//> Spock.var

-- | The route to the list of images.
imagesRoute :: Spock.Path '[String]
imagesRoute = Spock.var <//> "images"

-- | The route to the given page number from the album with the given ID.
pageRoute :: Spock.Path '[String, ID, Int]
pageRoute = Spock.var <//> "album" <//> Spock.var <//> Spock.var

-- | The route to upload a new post.
uploadRoute :: Spock.Path '[String]
uploadRoute = Spock.var <//> "upload"

------------------------------------------------------------- Routing Functions

-- | Returns the route to an album with the given ID, query, and scope.
album :: Maybe Scope -> ID -> Text
album scope id = Spock.renderRoute albumRoute (getScopeName scope) id

-- | Returns the route to the list of albums on the given page with the given
-- | query and scope.
albums :: Maybe Scope -> Int -> String -> Text
albums scope page query =
    let scopeName = getScopeName scope
        route     = Spock.renderRoute albumsRoute scopeName
        params    = [("page", if page <= 1 then "" else show page), ("q", query)]

    in withParameters route params

-- | Returns the route to the image with the given ID filtered by the given
-- | query and scope.
image :: Maybe Scope -> ID -> String -> Text
image scope id query =
    let scopeName = getScopeName scope
        route     = Spock.renderRoute imageRoute scopeName id
        params    = [("q", query)]

    in withParameters route params

-- | Returns the route to the list of images on the given page with the given
-- | query and scope.
images :: Maybe Scope -> Int -> String -> Text
images scope page query =
    let scopeName = getScopeName scope
        route     = Spock.renderRoute imagesRoute scopeName
        params    = [("page", if page <= 1 then "" else show page), ("q", query)]

    in route `withParameters` params

-- | Returns the route to the given page number from the album with the given
-- | ID and scope.
page :: Maybe Scope -> ID -> Int -> Text
page scope = Spock.renderRoute pageRoute (getScopeName scope)

-- | Returns the route to upload a new post.
upload :: Maybe Scope -> Text
upload scope = Spock.renderRoute uploadRoute (getScopeName scope)

----------------------------------------------------------------------- Utility

-- | Gets the scope name of the given scope or the default name if the given
-- | scope is nothing.
getScopeName :: Maybe Scope -> String
getScopeName = maybe Scope.defaultName scopeName

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
