{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiWayIf        #-}

module App.Web.View
    ( albumView, albumsView, imageView, imagesView, pageView, tagsView ) where

import qualified App.Core.Album   as Album
import qualified App.Core.Scope   as Scope
import qualified App.Path         as Path
import qualified App.Web.Element  as Elem
import qualified App.Web.Icon     as Icon
import qualified App.Web.URL      as URL
import qualified Data.Text        as Text
import qualified Data.Text.Lazy   as LazyText
import qualified Text.JavaScript  as JS

import App.Core.Types      ( Album(..), DetailedTag(..), Image(..), Page(..)
                           , Scope(..), Tag(..) )
import Control.Applicative ( (<|>) )
import Control.Monad       ( forM_, when )
import Data.List.Extended  ( groupWith )
import Data.Char           ( isAlpha, isNumber, toUpper )
import Data.Maybe          ( listToMaybe )
import Data.Monoid         ( (<>) )
import Data.Text           ( Text )
import Data.Textual        ( display, intercalate )
import Data.DateTime       ( TimeZone )
import Database.Engine     ( ID, Entity(..) )
import Lucid.Base          ( Html(..), renderText )

------------------------------------------------------------------------- Views

-- | Renders a view for the given album as text containing HTML.
albumView :: Scope -> String -> TimeZone -> Entity Album -> Text
albumView scope query timeZone entity @ (Entity id Album {..}) = render $ do
    let title    = Text.pack albumTitle
        album    = entityData entity
        onload   = JS.functionCall "AlbumViewModel.register" args
        args     = [ JS.toJSON (scopeName scope)
                   , JS.toJSON query
                   , JS.toJSON entity ]

    let indexURL = URL.albums scope 1 query
        albumURL = URL.album scope id

    Elem.document title onload $ do
        Elem.sideBar $ do
            Elem.actionLink Icon.Grid indexURL
        Elem.aside $ do
            Elem.infoPanel $ do
                Elem.actions $ do
                    Elem.actionGroup $ do
                        Elem.actionLink Icon.Grid indexURL
                    Elem.actionGroup $ do
                        Elem.action Icon.Trash  "delete-show"
                        Elem.action Icon.Pencil "edit-show"
                Elem.searchBox (URL.albums scope 1 "") query
                Elem.albumDetails album timeZone
                Elem.spacer
                Elem.albumTags scope albumTagNames
            Elem.editPanel albumURL $ do
                Elem.textBoxField  "Title" "title" "edit-title"
                Elem.textAreaField "Tags"  "tags"  "edit-tags"
            Elem.deletePanel albumURL
        Elem.gallery2 $
            flip map albumPages $ \page @ Page {..} ->
                ( URL.page scope id pageNumber
                , Path.getPageThumbnailURL id page)

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
                        Elem.actionLink Icon.LeftArrow (URL.albums scope (page - 1) query)
                Elem.actionGroup $ do
                    when nextAvailable $
                        Elem.actionLink Icon.RightArrow (URL.albums scope (page + 1) query)
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
            Elem.actionLink Icon.UpArrow   (URL.image  scope prev query)
            Elem.actionLink Icon.Grid      (URL.images scope 1    query)
            Elem.actionLink Icon.DownArrow (URL.image  scope next query)
            Elem.separator
            Elem.action Icon.Pause "toggle-double-compact"
        Elem.aside $ do
            Elem.infoPanel $ do
                Elem.actions $ do
                    Elem.actionGroup $ do
                        Elem.actionLink Icon.LeftArrow  (URL.image  scope prev query)
                        Elem.actionLink Icon.Grid       (URL.images scope 1    query)
                        Elem.actionLink Icon.RightArrow (URL.image  scope next query)
                    Elem.actionGroup $ do
                        Elem.action Icon.Trash  "delete-show"
                        Elem.action Icon.Pencil "edit-show"
                        Elem.action Icon.Pause  "toggle-double"
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
    let title       = "Images (" <> display total <> ")"
        onload      = JS.functionCall "ImagesViewModel.register" args
        args        = [ JS.toJSON (scopeName scope)
                      , JS.toJSON query
                      , JS.toJSON page
                      , JS.toJSON canPrevious
                      , JS.toJSON canNext ]

        firstID     = entityID <$> listToMaybe images
        canPrevious = page > 1
        canNext     = page * pageSize < total

        tagsURL         = URL.tags scope
        previousPageURL = URL.images scope (page - 1) query
        firstPageURL    = URL.images scope 1          query
        nextPageURL     = URL.images scope (page + 1) query
        albumsURL       = URL.albums scope 1          query
        firstResultURL  = URL.image scope <$> firstID <*> pure query

        previousPageAction icon =
            if canPrevious
                then Elem.actionLink     icon previousPageURL
                else Elem.disabledAction icon

        nextPageAction icon =
            if canNext
                then Elem.actionLink     icon nextPageURL
                else Elem.disabledAction icon

        firstResultAction =
            case firstResultURL of
                Just url -> Elem.actionLink     Icon.LevelDown url
                Nothing  -> Elem.disabledAction Icon.LevelDown

    Elem.document title onload $ do
        Elem.sideBar $ do
            previousPageAction Icon.UpArrow
            firstResultAction
            nextPageAction     Icon.DownArrow
            Elem.separator
            Elem.actionLink    Icon.Book albumsURL
            Elem.separator
            Elem.actionLink    Icon.Tags tagsURL
        Elem.aside $ do
            Elem.infoPanel $ do
                Elem.actions $ do
                    Elem.actionGroup $ do
                        previousPageAction Icon.LeftArrow
                        firstResultAction
                        nextPageAction     Icon.RightArrow
                    Elem.actionGroup $ do
                        Elem.actionLink Icon.Tags tagsURL
                        Elem.actionLink Icon.Book albumsURL
                Elem.searchBox firstPageURL query
                Elem.spacer
                Elem.uploadForm scope
        Elem.gallery2 $
            flip map images $ \(Entity id image) ->
                ( URL.image scope id query
                , Path.getImageThumbnailURL image)

-- | Renders a view for the given page of the given album as text containing
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

-- | Renders a view for the list of tags as text containing HTML.
tagsView :: Scope -> [Entity DetailedTag] -> Text
tagsView scope tagEntities = render $ do
    let title  = "Tags"
        onload = JS.functionCall "TagsViewModel.register" args
        args   = [ JS.toJSON (scopeName scope) ]

    let tags               = map entityData tagEntities
        groups             = groupWith getGroupHeader tags
        getGroupHeader tag = let char = head (detailedTagName tag)
                             in if | isNumber char -> "#"
                                   | isAlpha  char -> [toUpper char]
                                   | otherwise     -> "Symbol"

    Elem.document title onload $ do
        Elem.tagList $ do
            forM_ groups $ \group -> do
                Elem.tagHeader (getGroupHeader (head group))
                forM_ group $ \tag -> do
                    Elem.tagDetail scope tag

----------------------------------------------------------------------- Utility

-- | Renders the given HTML as text.
render :: Html () -> Text
render = LazyText.toStrict . renderText
