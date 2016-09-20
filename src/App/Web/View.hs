{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Web.View ( albumView, albumsView, imageView, imagesView, pageView ) where

import qualified App.Core.Scope   as Scope
import qualified App.Path         as Path
import qualified App.Web.Route    as Route
import qualified App.Web.Element  as Elem
import qualified Data.Text        as Text
import qualified Data.Text.Lazy   as LazyText
import qualified Text.JavaScript  as JS

import App.Core.Types  ( Album(..), Image(..), Page(..), Scope(..) )
import Control.Monad   ( when )
import Data.Monoid     ( (<>) )
import Data.Text       ( Text )
import Data.Textual    ( display, intercalate )
import Data.DateTime   ( TimeZone )
import Database.Engine ( ID, Entity(..) )
import Lucid.Base      ( Html(..), renderText )

------------------------------------------------------------------------- Views

-- | Renders a view for the given album as text containing HTML.
albumView :: (Maybe Scope) -> String -> TimeZone -> Entity Album -> Text
albumView scope query timeZone (Entity id album @ Album{..}) = render $ do
    let title  = "Album " <> display id
        onload = JS.functionCall "Album.initializePage" args
        args   = [ JS.toJSON (maybe "" scopeName scope)
                 , JS.toJSON id
                 , JS.toJSON query ]

    Elem.document title onload $ do
        Elem.aside $ Elem.infoPanel $ do
            Elem.actions $ do
                Elem.actionGroup $ do
                    Elem.actionLink Elem.Grid (Route.albums scope 1 query)
                Elem.actionGroup $ do
                    Elem.action Elem.Pencil "edit-show"
                    Elem.action Elem.Trash  "delete"
            Elem.albumDetails album timeZone
            Elem.albumTags scope albumTagNames
        Elem.gallery $
            flip map albumPages $ \page @ Page {..} ->
                ( Route.page scope id pageNumber
                , Path.getPageThumbnailURL id page)
        Elem.editForm (Route.album scope id) $ do
            Elem.textBoxField  "Title" "title" "edit-title"
            Elem.textAreaField "Tags"  "tags"  "edit-tags"

-- | Renders an index view for albums as text containing HTML.
albumsView :: Maybe Scope -> String -> Int -> Int -> Int -> [Entity Album] -> Text
albumsView scope query page total pageSize albums = render $ do
    let prevAvailable = page > 1
        nextAvailable = page * pageSize < total
        title         = "Albums (" <> display total <> ")"
        onload        = JS.functionCall "Albums.initializePage" args
        args          = [ JS.toJSON (maybe "" scopeName scope)
                        , JS.toJSON prevAvailable
                        , JS.toJSON nextAvailable
                        , JS.toJSON page
                        , JS.toJSON query ]

    Elem.document title onload $ do
        Elem.aside $ Elem.infoPanel $ do
            Elem.searchBox (Route.albums scope 1 "") query
            Elem.actions $ do
                Elem.actionGroup $ do
                    when prevAvailable $
                        Elem.actionLink Elem.LeftArrow (Route.albums scope (page - 1) query)
                Elem.actionGroup $ do
                    when nextAvailable $
                        Elem.actionLink Elem.RightArrow (Route.albums scope (page + 1) query)
            Elem.spacer
            Elem.uploadForm scope
        Elem.gallery $
            flip map albums $ \(Entity id album) ->
                ( Route.album scope id
                , Path.getAlbumThumbnailURL (Entity id album))

-- | Renders a view for the given image as text containing HTML.
imageView :: Maybe Scope -> String -> TimeZone -> Entity Image -> Entity Image -> Entity Image -> Text
imageView scope query timeZone (Entity prev _) (Entity curr image) (Entity next _) = render $ do
    let title  = "Image " <> display curr
        source = Text.pack (Path.getImageURL image)
        onload = JS.functionCall "ImageViewModel.register" args
        args   = [ JS.toJSON (maybe Scope.defaultName scopeName scope)
                 , JS.toJSON query
                 , JS.toJSON prev
                 , JS.toJSON curr
                 , JS.toJSON next ]

    Elem.document title onload $ do
        Elem.aside $ do
            Elem.infoPanel $ do
                Elem.actions $ do
                    Elem.actionGroup $ do
                        Elem.actionLink Elem.LeftArrow  (Route.image  scope prev query)
                        Elem.actionLink Elem.Grid       (Route.images scope 1    query)
                        Elem.actionLink Elem.RightArrow (Route.image  scope next query)
                    Elem.actionGroup $ do
                        Elem.action Elem.Pencil "edit-show"
                        Elem.action Elem.Trash  "delete-show"
                Elem.searchBox (Route.images scope 1 "") query
                Elem.imageDetails image timeZone
                Elem.imageTags scope (imageTagNames image)
            Elem.editPanel (Route.image scope curr query) $ do
                Elem.textBoxField  "Title" "title" "edit-title"
                Elem.textAreaField "Tags"  "tags"  "edit-tags"
            Elem.deletePanel (Route.image scope curr query)
        if isVideo (imageExtension image)
            then Elem.video source
            else Elem.image source

-- | Renders an index view for images as text containing HTML.
imagesView :: Maybe Scope -> String -> Int -> Int -> Int -> [Entity Image] -> Text
imagesView scope query page total pageSize images = render $ do
    let prevAvailable = page > 1
        nextAvailable = page * pageSize < total
        title         = "Images (" <> display total <> ")"
        onload        = JS.functionCall "Images.initializePage" args
        args          = [ JS.toJSON (maybe "" scopeName scope)
                        , JS.toJSON prevAvailable
                        , JS.toJSON nextAvailable
                        , JS.toJSON page
                        , JS.toJSON query ]

    Elem.document title onload $ do
        Elem.aside $ Elem.infoPanel $ do
            Elem.searchBox (Route.images scope 1 "") query
            Elem.actions $ do
                Elem.actionGroup $ do
                    when prevAvailable $
                        Elem.actionLink Elem.LeftArrow (Route.images scope (page - 1) query)
                Elem.actionGroup $ do
                    when nextAvailable $
                        Elem.actionLink Elem.RightArrow (Route.images scope (page + 1) query)
            Elem.spacer
            Elem.uploadForm scope
        Elem.gallery $
            flip map images $ \(Entity id image) ->
                ( Route.image scope id query
                , Path.getImageThumbnailURL image)

-- | Renders a view for the give page of the given album as text containing
-- | HTML.
pageView :: Scope -> Entity Album -> Page -> Text
pageView Scope {..} (Entity id Album {..}) page = render $ do
    let number = pageNumber page
        pages  = length albumPages
        source = Text.pack (Path.getPageURL id page)
        title  = Text.pack (albumTitle ++ " (" ++ show number ++ "/" ++ show pages ++ ")")
        prev   = if number <= 1 then pages else number - 1
        next   = if number >= pages then 1 else number + 1
        onload = JS.functionCall "Page.initializePage" args
        args   = [ JS.toJSON scopeName
                 , JS.toJSON id
                 , JS.toJSON prev
                 , JS.toJSON next ]

    Elem.document title onload $ do
        if isVideo (pageExtension page)
            then Elem.video source
            else Elem.image source

----------------------------------------------------------------------- Utility

-- | Returns whether or not the given file extension represents a video.
isVideo :: String -> Bool
isVideo "webm" = True
isVideo _      = False

-- | Renders the given HTML as text.
render :: Html () -> Text
render = LazyText.toStrict . renderText
