{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Web.View ( albumView, albumsView, imageView, imagesView, pageView ) where

import qualified App.Path         as Path
import qualified App.Web.Route    as Route
import qualified App.Web.Element  as Elem
import qualified Data.Text        as Text
import qualified Data.Text.Lazy   as LazyText
import qualified Text.JavaScript  as JS

import App.Core.Types  ( Album(..), Image(..), Page(..) )
import Control.Monad   ( when )
import Data.Monoid     ( (<>) )
import Data.Text       ( Text )
import Data.Textual    ( display, intercalate )
import Data.DateTime   ( TimeZone )
import Database.Engine ( ID, Entity(..) )
import Lucid.Base      ( Html(..), renderText )

------------------------------------------------------------------------- Views

-- | Renders a view for the given album as text containing HTML.
albumView :: String -> TimeZone -> Entity Album -> Text
albumView query timeZone (Entity id album @ Album{..}) = render $ do
    let title  = "Album " <> display id
        onload = JS.functionCall "Album.initializePage" args
        args   = [ JS.toJSON id
                 , JS.toJSON query ]

    Elem.document title onload $ do
        Elem.sidePanel $ do
            Elem.actions $ do
                Elem.actionGroup $ do
                    Elem.actionLink Elem.Grid (Route.albums 1 query)
                Elem.actionGroup $ do
                    Elem.action Elem.Pencil "edit-show"
                    Elem.action Elem.Trash  "delete"
            Elem.albumDetails album timeZone
            Elem.tags (albumTagNames)
        Elem.gallery $
            flip map albumPages $ \page @ Page {..} ->
                ( Route.page id pageNumber
                , Path.getPageThumbnailURL id page)
        Elem.editForm (Route.album id) $ do
            Elem.textBoxField  "Title" "title" "edit-title"
            Elem.textAreaField "Tags"  "tags"  "edit-tags"

-- | Renders an index view for albums as text containing HTML.
albumsView :: String -> Int -> Int -> Int -> [Entity Album] -> Text
albumsView query page total pageSize albums = render $ do
    let prevAvailable = page > 1
        nextAvailable = page * pageSize < total
        title         = "Albums (" <> display total <> ")"
        onload        = JS.functionCall "Albums.initializePage" args
        args          = [ JS.toJSON prevAvailable
                        , JS.toJSON nextAvailable
                        , JS.toJSON page
                        , JS.toJSON query ]

    Elem.document title onload $ do
    Elem.sidePanel $ do
        Elem.searchBox Elem.AlbumSearch query
        Elem.actions $ do
            Elem.actionGroup $ do
                when prevAvailable $
                    Elem.actionLink Elem.LeftArrow (Route.albums (page - 1) query)
            Elem.actionGroup $ do
                when nextAvailable $
                    Elem.actionLink Elem.RightArrow (Route.albums (page + 1) query)
        Elem.spacer
        Elem.uploadForm
    Elem.gallery $
        flip map albums $ \(Entity id album) ->
            ( Route.album id
            , Path.getAlbumThumbnailURL (Entity id album))

-- | Renders a view for the given image as text containing HTML.
imageView :: String -> TimeZone -> Entity Image -> Entity Image -> Entity Image -> Text
imageView query timeZone (Entity prev _) (Entity curr image) (Entity next _) = render $ do
    let title  = "Image " <> display curr
        source = Text.pack (Path.getImageURL image)
        onload = JS.functionCall "Image.initializePage" args
        args   = [ JS.toJSON prev
                 , JS.toJSON curr
                 , JS.toJSON next
                 , JS.toJSON query ]

    Elem.document title onload $ do
        Elem.sidePanel $ do
            Elem.searchBox Elem.ImageSearch query
            Elem.actions $ do
                Elem.actionGroup $ do
                    Elem.actionLink Elem.LeftArrow  (Route.image  prev query)
                    Elem.actionLink Elem.Grid       (Route.images 1    query)
                    Elem.actionLink Elem.RightArrow (Route.image  next query)
                Elem.actionGroup $ do
                    Elem.action Elem.Pencil "edit-show"
                    Elem.action Elem.Trash  "delete"
            Elem.imageDetails image timeZone
            Elem.tags (imageTagNames image)
        if isVideo image
            then Elem.video source
            else Elem.image source
        Elem.editForm (Route.image curr "") $ do
            Elem.textBoxField  "Title" "title" "edit-title"
            Elem.textAreaField "Tags"  "tags"  "edit-tags"

-- | Renders an index view for images as text containing HTML.
imagesView :: String -> Int -> Int -> Int -> [Entity Image] -> Text
imagesView query page total pageSize images = render $ do
    let prevAvailable = page > 1
        nextAvailable = page * pageSize < total
        title         = "Images (" <> display total <> ")"
        onload        = JS.functionCall "Images.initializePage" args
        args          = [ JS.toJSON prevAvailable
                        , JS.toJSON nextAvailable
                        , JS.toJSON page
                        , JS.toJSON query ]

    Elem.document title onload $ do
    Elem.sidePanel $ do
        Elem.searchBox Elem.ImageSearch query
        Elem.actions $ do
            Elem.actionGroup $ do
                when prevAvailable $
                    Elem.actionLink Elem.LeftArrow (Route.images (page - 1) query)
            Elem.actionGroup $ do
                when nextAvailable $
                    Elem.actionLink Elem.RightArrow (Route.images (page + 1) query)
        Elem.spacer
        Elem.uploadForm
    Elem.gallery $
        flip map images $ \(Entity id image) ->
            ( Route.image id query
            , Path.getImageThumbnailURL image)

-- | Renders a view for the give page of the given album as text containing
-- | HTML.
pageView :: ID -> Page -> Text
pageView id page = render $ do
    let title  = "page " <> display page
        onload = JS.functionCall "Page.initializePage" []

    Elem.document title onload $ do
        Elem.image (Text.pack (Path.getPageURL id page))

----------------------------------------------------------------------- Utility

-- | Returns whether or not the given image is a video.
isVideo :: Image -> Bool
isVideo Image {..} = imageExtension == "webm"

-- | Renders the given HTML as text.
render :: Html () -> Text
render = LazyText.toStrict . renderText
