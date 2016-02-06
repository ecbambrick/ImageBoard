{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.View ( albumView, albumsView, imageView, imagesView, pageView ) where

import qualified App.Path         as Path
import qualified App.View.Element as Elem
import qualified Network.URI      as URI
import qualified Data.Text        as Text
import qualified Data.Text.Lazy   as LazyText
import qualified Text.JavaScript  as JS

import App.Common       ( Album(..), Image(..), Page(..) )
import Control.Monad    ( when )
import Data.Monoid      ( (<>) )
import Data.Text        ( Text )
import Data.Textual     ( display, intercalate )
import Data.DateTime    ( TimeZone )
import Database.Engine  ( ID, Entity(..) )
import Lucid.Base       ( Html(..), renderText )

------------------------------------------------------------------------- Views

-- | Renders a view for the given album as text containing HTML.
albumView :: String -> TimeZone -> Entity Album -> Text
albumView query timeZone (Entity id album @ Album{..}) =

    let title  = "Album " <> display id
        onload = JS.functionCall "Album.initializePage" args
        args   = [ JS.toJSON id
                 , JS.toJSON query ]

    in render $
        Elem.document title onload $ do
            Elem.sidePanel $ do
                Elem.actions $ do
                    Elem.actionGroup $ do
                        Elem.actionLink Elem.Grid (albumsURL 1 query)
                    Elem.actionGroup $ do
                        Elem.action Elem.Pencil "edit-show"
                        Elem.action Elem.Trash  "delete"
                Elem.albumDetails album timeZone
                Elem.tags (albumTagNames)
            Elem.gallery $
                flip map albumPages $ \page @ Page {..} ->
                    ( pageURL id pageNumber
                    , Path.getPageThumbnailURL id page)
            Elem.editForm (albumURL id "") $ do
                Elem.textBoxField  "Title" "title" "edit-title"
                Elem.textAreaField "Tags"  "tags"  "edit-tags"

-- | Renders an index view for albums as text containing HTML.
albumsView :: String -> Int -> Int -> Int -> [Entity Album] -> Text
albumsView query page total pageSize albums =

    let prevAvailable = page > 1
        nextAvailable = page * pageSize < total
        title         = "Albums (" <> display total <> ")"
        onload        = JS.functionCall "Albums.initializePage" args
        args          = [ JS.toJSON prevAvailable
                        , JS.toJSON nextAvailable
                        , JS.toJSON page
                        , JS.toJSON query ]

    in render $
        Elem.document title onload $ do
        Elem.sidePanel $ do
            Elem.searchBox Elem.AlbumSearch query
            Elem.actions $ do
                Elem.actionGroup $ do
                    when prevAvailable $
                        Elem.actionLink Elem.LeftArrow (albumsURL (page - 1) query)
                Elem.actionGroup $ do
                    when nextAvailable $
                        Elem.actionLink Elem.RightArrow (albumsURL (page + 1) query)
            Elem.spacer
            Elem.uploadForm
        Elem.gallery $
            flip map albums $ \(Entity id album) ->
                ( albumURL id query
                , Path.getAlbumThumbnailURL (Entity id album))

-- | Renders a view for the given image as text containing HTML.
imageView :: String -> TimeZone -> Entity Image -> Entity Image -> Entity Image -> Text
imageView query timeZone (Entity prev _) (Entity curr image) (Entity next _) =

    let title  = "Image " <> display curr
        source = Text.pack (Path.getImageURL image)
        onload = JS.functionCall "Image.initializePage" args
        args   = [ JS.toJSON prev
                 , JS.toJSON curr
                 , JS.toJSON next
                 , JS.toJSON query ]

    in render $
        Elem.document title onload $ do
            Elem.sidePanel $ do
                Elem.searchBox Elem.ImageSearch query
                Elem.actions $ do
                    Elem.actionGroup $ do
                        Elem.actionLink Elem.LeftArrow  (imageURL  prev query)
                        Elem.actionLink Elem.Grid       (imagesURL 1    query)
                        Elem.actionLink Elem.RightArrow (imageURL  next query)
                    Elem.actionGroup $ do
                        Elem.action Elem.Pencil "edit-show"
                        Elem.action Elem.Trash  "delete"
                Elem.imageDetails image timeZone
                Elem.tags (imageTagNames image)
            if isVideo image
                then Elem.video source
                else Elem.image source
            Elem.editForm (imageURL curr "") $ do
                Elem.textBoxField  "Title" "title" "edit-title"
                Elem.textAreaField "Tags"  "tags"  "edit-tags"

-- | Renders an index view for images as text containing HTML.
imagesView :: String -> Int -> Int -> Int -> [Entity Image] -> Text
imagesView query page total pageSize images =

    let prevAvailable = page > 1
        nextAvailable = page * pageSize < total
        title         = "Images (" <> display total <> ")"
        onload        = JS.functionCall "Images.initializePage" args
        args          = [ JS.toJSON prevAvailable
                        , JS.toJSON nextAvailable
                        , JS.toJSON page
                        , JS.toJSON query ]

    in render $
        Elem.document title onload $ do
        Elem.sidePanel $ do
            Elem.searchBox Elem.ImageSearch query
            Elem.actions $ do
                Elem.actionGroup $ do
                    when prevAvailable $
                        Elem.actionLink Elem.LeftArrow (imagesURL (page - 1) query)
                Elem.actionGroup $ do
                    when nextAvailable $
                        Elem.actionLink Elem.RightArrow (imagesURL (page + 1) query)
            Elem.spacer
            Elem.uploadForm
        Elem.gallery $
            flip map images $ \(Entity id image) ->
                ( imageURL id query
                , Path.getImageThumbnailURL image)

-- | Renders a view for the give page of the given album as text containing
-- | HTML.
pageView :: ID -> Page -> Text
pageView id page =

    let title  = "page " <> display page
        onload = JS.functionCall "Page.initializePage" []

    in render $
        Elem.document title onload $ do
            Elem.image (Text.pack (Path.getPageURL id page))

----------------------------------------------------------------------- Utility

albumURL :: ID -> String -> Text
albumURL id query = "/album/" <> display id <> parameters [("q", query)]

albumsURL :: Int -> String -> Text
albumsURL page query = "/albums/" <> parameters [ ("page", show page), ("q", query) ]

-- | Returns a relative URL for the image view with the given ID and query.
imageURL :: ID -> String -> Text
imageURL id query = "/image/" <> display id <> parameters [("q", query)]

-- | Returns a relative URL for the images view with for the given page number
-- | and query.
imagesURL :: Int -> String -> Text
imagesURL page query = "/images/" <> parameters [ ("page", show page), ("q", query) ]

-- | Returns a relative URL for the page view for the given album ID and page
-- | number.
pageURL :: ID -> Int -> Text
pageURL id page = "/album/" <> display id <> "/" <> display page

-- | Returns whether or not the given image is a video.
isVideo :: Image -> Bool
isVideo Image {..} = imageExtension == "webm"

-- | Converts the given list of key-value pairs to a set of parameter values.
parameters :: [(String, String)] -> Text
parameters params =
    let escape             = Text.pack . URI.escapeURIString URI.isUnreserved
        encodePair ("", _) = ""
        encodePair (_, "") = ""
        encodePair (k,  v) = escape k <> "=" <> escape v
        encodedParams      = filter (not . Text.null) $ map encodePair params
        results            = intercalate "&" encodedParams

    in if Text.null results then "" else "?" <> results

-- | Renders the given HTML as text.
render :: Html () -> Text
render = LazyText.toStrict . renderText
