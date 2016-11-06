{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Web.View
    ( albumView, albumsView, imageView, imagesView, pageView ) where

import qualified App.Core.Album   as Album
import qualified App.Core.Scope   as Scope
import qualified App.Path         as Path
import qualified App.Web.Element  as Elem
import qualified App.Web.URL      as URL
import qualified Data.Text        as Text
import qualified Data.Text.Lazy   as LazyText
import qualified Text.JavaScript  as JS

import App.Core.Types      ( Album(..), Image(..), Page(..), Scope(..) )
import Control.Applicative ( (<|>) )
import Control.Monad       ( when )
import Data.Monoid         ( (<>) )
import Data.Text           ( Text )
import Data.Textual        ( display, intercalate )
import Data.DateTime       ( TimeZone )
import Database.Engine     ( ID, Entity(..) )
import Lucid.Base          ( Html(..), renderText )

------------------------------------------------------------------------- Views

-- | Renders a view for the given album as text containing HTML.
albumView :: Scope -> String -> TimeZone -> Entity Album -> Text
albumView scope query timeZone (Entity id album @ Album{..}) = render $ do
    let title  = "Album " <> display id
        onload = JS.functionCall "Album.initializePage" args
        args   = [ JS.toJSON (scopeName scope)
                 , JS.toJSON id
                 , JS.toJSON query ]

    Elem.document title onload $ do
        Elem.aside $ Elem.infoPanel $ do
            Elem.actions $ do
                Elem.actionGroup $ do
                    Elem.actionLink Elem.Grid (URL.albums scope 1 query)
                Elem.actionGroup $ do
                    Elem.action Elem.Pencil "edit-show"
                    Elem.action Elem.Trash  "delete"
            Elem.albumDetails album timeZone
            Elem.albumTags scope albumTagNames
        Elem.gallery $
            flip map albumPages $ \page @ Page {..} ->
                ( URL.page scope id pageNumber
                , Path.getPageThumbnailURL id page)
        Elem.editForm (URL.album scope id) $ do
            Elem.textBoxField  "Title" "title" "edit-title"
            Elem.textAreaField "Tags"  "tags"  "edit-tags"

-- | Renders an index view for albums as text containing HTML.
albumsView :: Scope -> String -> Int -> Int -> Int -> [Entity Album] -> Text
albumsView scope query page total pageSize albums = render $ do
    let prevAvailable = page > 1
        nextAvailable = page * pageSize < total
        title         = "Albums (" <> display total <> ")"
        onload        = JS.functionCall "Albums.initializePage" args
        args          = [ JS.toJSON (scopeName scope)
                        , JS.toJSON prevAvailable
                        , JS.toJSON nextAvailable
                        , JS.toJSON page
                        , JS.toJSON query ]

    Elem.document title onload $ do
        Elem.aside $ Elem.infoPanel $ do
            Elem.searchBox (URL.albums scope 1 "") query
            Elem.actions $ do
                Elem.actionGroup $ do
                    when prevAvailable $
                        Elem.actionLink Elem.LeftArrow (URL.albums scope (page - 1) query)
                Elem.actionGroup $ do
                    when nextAvailable $
                        Elem.actionLink Elem.RightArrow (URL.albums scope (page + 1) query)
            Elem.spacer
            Elem.uploadForm scope
        Elem.gallery $
            flip map albums $ \(Entity id album) ->
                ( URL.album scope id
                , Path.getAlbumThumbnailURL (Entity id album))

-- | Renders a view for the given image as text containing HTML.
imageView :: Scope -> String -> TimeZone -> Entity Image -> Entity Image -> Entity Image -> Text
imageView scope query timeZone previousImage currentImage nextImage = render $ do
    let prev    = entityID previousImage
        curr    = entityID currentImage
        next    = entityID nextImage
        image1  = entityData currentImage
        image2  = entityData nextImage
        title   = "Image " <> display curr
        source1 = Path.getImageURL image1
        source2 = Path.getImageURL image2
        onload  = JS.functionCall "ImageViewModel.register" args
        args    = [ JS.toJSON (scopeName scope)
                  , JS.toJSON query
                  , JS.toJSON previousImage
                  , JS.toJSON currentImage
                  , JS.toJSON nextImage ]

    Elem.document title onload $ do
        Elem.sideBar $ do
            Elem.actionLink Elem.UpArrow   (URL.image  scope prev query)
            Elem.actionLink Elem.Grid      (URL.images scope 1    query)
            Elem.actionLink Elem.DownArrow (URL.image  scope next query)
            Elem.separator
            Elem.action Elem.Pause "toggle-double-compact"
        Elem.aside $ do
            Elem.infoPanel $ do
                Elem.actions $ do
                    Elem.actionGroup $ do
                        Elem.actionLink Elem.LeftArrow  (URL.image  scope prev query)
                        Elem.actionLink Elem.Grid       (URL.images scope 1    query)
                        Elem.actionLink Elem.RightArrow (URL.image  scope next query)
                    Elem.actionGroup $ do
                        Elem.action Elem.Trash  "delete-show"
                        Elem.action Elem.Pencil "edit-show"
                        Elem.action Elem.Pause  "toggle-double"
                Elem.searchBox (URL.images scope 1 "") query
                Elem.imageDetails image1 timeZone Elem.MainImage
                Elem.imageDetails image2 timeZone Elem.SecondaryImage
                Elem.spacer
                Elem.imageTags scope (imageTagNames image1)
            Elem.editPanel (URL.image scope curr query) $ do
                Elem.textBoxField  "Title" "title" "edit-title"
                Elem.textAreaField "Tags"  "tags"  "edit-tags"
            Elem.deletePanel (URL.image scope curr query)
        Elem.display source1 source2

-- | Renders an index view for images as text containing HTML.
imagesView :: Scope -> String -> Int -> Int -> Int -> [Entity Image] -> Text
imagesView scope query page total pageSize images = render $ do
    let prevAvailable = page > 1
        nextAvailable = page * pageSize < total
        title         = "Images (" <> display total <> ")"
        onload        = JS.functionCall "Images.initializePage" args
        args          = [ JS.toJSON (scopeName scope)
                        , JS.toJSON prevAvailable
                        , JS.toJSON nextAvailable
                        , JS.toJSON page
                        , JS.toJSON query ]

    Elem.document title onload $ do
        Elem.aside $ Elem.infoPanel $ do
            Elem.searchBox (URL.images scope 1 "") query
            Elem.actions $ do
                Elem.actionGroup $ do
                    when prevAvailable $
                        Elem.actionLink Elem.LeftArrow (URL.images scope (page - 1) query)
                Elem.actionGroup $ do
                    when nextAvailable $
                        Elem.actionLink Elem.RightArrow (URL.images scope (page + 1) query)
            Elem.spacer
            Elem.uploadForm scope
        Elem.gallery $
            flip map images $ \(Entity id image) ->
                ( URL.image scope id query
                , Path.getImageThumbnailURL image)

-- | Renders a view for the give page of the given album as text containing
-- | HTML.
pageView :: Scope -> Entity Album -> Int -> Text
pageView scope (Entity id album) number = render $ do
    let pages  = length (albumPages album)
        page   = Album.getPage album number <|> Album.getPage album 1
        prev   = if number <= 1 || number > pages then pages else number - 1
        next   = if number >= pages then 1 else number + 1
        title  = Text.pack (albumTitle album ++ " (" ++ show number ++ "/" ++ show pages ++ ")")
        onload = JS.functionCall "Page.initializePage" args
        args   = [ JS.toJSON (scopeName scope)
                 , JS.toJSON id
                 , JS.toJSON prev
                 , JS.toJSON next ]

    Elem.document title onload $ do
        case page of
            Nothing   -> mempty
            Just page -> Elem.display (Path.getPageURL id page) ""

----------------------------------------------------------------------- Utility

-- | Renders the given HTML as text.
render :: Html () -> Text
render = LazyText.toStrict . renderText
