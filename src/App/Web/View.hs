{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Web.View
    ( albumView, albumsView, imageView, imagesView, pageView, tagsView) where

import qualified App.Core.Album   as Album
import qualified App.Core.Scope   as Scope
import qualified App.Web.Element  as Elem
import qualified App.Web.Icon     as Icon
import qualified App.Web.URL      as URL
import qualified Data.Text        as Text
import qualified Data.Text.Lazy   as LazyText
import qualified Text.JavaScript  as JS

import App.Core.Types     ( Album(..), DetailedTag(..), Image(..), Page(..), Scope(..) )
import App.Web.URL        ( TagGrouping(..) )
import Control.Monad      ( forM_ )
import Data.Maybe         ( listToMaybe )
import Data.Monoid        ( (<>) )
import Data.Text          ( Text )
import Data.Textual       ( display )
import Data.DateTime      ( TimeZone )
import Lucid.Base         ( Html(..), renderText )
import Web.PathPieces     ( PathPiece(..) )

------------------------------------------------------------------------- Views

-- | Renders a view for the given album as text containing HTML.
albumView :: Scope -> String -> TimeZone -> Album -> Text
albumView scope query timeZone album @ Album {..} = render $ do
    let title    = Text.pack albumTitle
        onload   = JS.functionCall "AlbumViewModel.register" args
        args     = [ JS.toJSON (scopeName scope)
                   , JS.toJSON query
                   , JS.toJSON album ]

    let indexURL = URL.albums scope 1       query
        albumURL = URL.album  scope albumID query

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
                Elem.albumTags scope albumTags
            Elem.editPanel albumURL $ do
                Elem.textBoxField  "Title" "title" "edit-title"
                Elem.textAreaField "Tags"  "tags"  "edit-tags"
            Elem.deletePanel albumURL
        Elem.gallery2 $
            flip map albumPages $ \page @ Page {..} ->
                ( URL.page scope albumID pageNumber query
                , URL.pageThumb albumID page)

-- | Renders an index view for albums as text containing HTML.
albumsView :: Scope -> String -> Int -> Int -> Int -> [Album] -> Text
albumsView scope query page total pageSize albums = render $ do
    let title       = "Albums (" <> display total <> ")"
        onload      = JS.functionCall "AlbumsViewModel.register" args
        args        = [ JS.toJSON (scopeName scope)
                      , JS.toJSON query
                      , JS.toJSON page
                      , JS.toJSON canPrevious
                      , JS.toJSON canNext ]

        firstID     = albumID <$> listToMaybe albums
        canPrevious = page > 1
        canNext     = page * pageSize < total

        tagsURL         = URL.tags   scope query Nothing
        previousPageURL = URL.albums scope (page - 1) query
        firstPageURL    = URL.albums scope 1          query
        nextPageURL     = URL.albums scope (page + 1) query
        imagesURL       = URL.images scope 1          query
        firstResultURL  = URL.album  scope <$> firstID <*> pure query

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
            Elem.actionLink    Icon.Image imagesURL
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
                        Elem.actionLink Icon.Image imagesURL
                Elem.searchBox firstPageURL query
                Elem.spacer
                Elem.uploadForm scope
        Elem.albumGallery scope query albums


-- | Renders a view for the given image as text containing HTML.
imageView :: Scope -> String -> TimeZone -> Image -> Image -> Image -> Text
imageView scope query timeZone prevImage currImage nextImage = render $ do
    let prev    = imageID prevImage
        curr    = imageID currImage
        next    = imageID nextImage
        title   = "Image " <> display curr
        source1 = URL.imageFile currImage
        source2 = URL.imageFile nextImage
        onload  = JS.functionCall "ImageViewModel.register" args
        args    = [ JS.toJSON (scopeName scope)
                  , JS.toJSON query
                  , JS.toJSON prevImage
                  , JS.toJSON currImage
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
                Elem.imageDetails currImage timeZone Elem.MainImage
                Elem.imageDetails nextImage timeZone Elem.SecondaryImage
                Elem.spacer
                Elem.imageTags scope (imageTags currImage)
            Elem.editPanel (URL.image scope curr query) $ do
                Elem.textBoxField  "Title" "title" "edit-title"
                Elem.textAreaField "Tags"  "tags"  "edit-tags"
            Elem.deletePanel (URL.image scope curr query)
        Elem.canvas source1 source2

-- | Renders an index view for images as text containing HTML.
imagesView :: Scope -> String -> Int -> Int -> Int -> [Image] -> Text
imagesView scope query page total pageSize images = render $ do
    let title       = "Images (" <> display total <> ")"
        onload      = JS.functionCall "ImagesViewModel.register" args
        args        = [ JS.toJSON (scopeName scope)
                      , JS.toJSON query
                      , JS.toJSON page
                      , JS.toJSON canPrevious
                      , JS.toJSON canNext ]

        firstID     = imageID <$> listToMaybe images
        canPrevious = page > 1
        canNext     = page * pageSize < total

        tagsURL         = URL.tags   scope query Nothing
        previousPageURL = URL.images scope (page - 1) query
        firstPageURL    = URL.images scope 1          query
        nextPageURL     = URL.images scope (page + 1) query
        albumsURL       = URL.albums scope 1          query
        firstResultURL  = URL.image  scope <$> firstID <*> pure query

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
        Elem.imageGallery scope query images

-- | Renders a view for the given page of the given album as text containing
-- | HTML.
pageView :: Scope -> Album -> TimeZone -> Int -> String -> Text
pageView scope album @ Album {..} timeZone curr query = render $ do
    let title  = Text.pack (albumTitle ++ " (" ++ show curr ++ "/" ++ show pages ++ ")")
        onload = JS.functionCall "PageViewModel.register" args
        args   = [ JS.toJSON (scopeName scope)
                 , JS.toJSON query
                 , JS.toJSON album
                 , JS.toJSON prev
                 , JS.toJSON next]

        pages = length albumPages
        prev  = curr - 1 `rollOver` length albumPages
        next  = curr + 1 `rollOver` length albumPages

        albumURL    = URL.album scope albumID      query
        prevPageURL = URL.page  scope albumID prev query
        nextPageURL = URL.page  scope albumID next query

        source1  = maybe "" (URL.pageFile albumID) (Album.getPage album curr)
        source2  = maybe "" (URL.pageFile albumID) (Album.getPage album next)

    Elem.document title onload $ do
        Elem.sideBar $ do
            Elem.actionLink Icon.UpArrow   prevPageURL
            Elem.actionLink Icon.Grid      albumURL
            Elem.actionLink Icon.DownArrow nextPageURL
            Elem.separator
            Elem.action Icon.Pause "toggle-double-compact"
        Elem.aside $ do
            Elem.infoPanel $ do
                Elem.actions $ do
                    Elem.actionGroup $ do
                        Elem.actionLink Icon.LeftArrow  prevPageURL
                        Elem.actionLink Icon.Grid       albumURL
                        Elem.actionLink Icon.RightArrow nextPageURL
                    Elem.actionGroup $ do
                        Elem.action Icon.Trash  "delete-show"
                        Elem.action Icon.Pencil "edit-show"
                        Elem.action Icon.Pause  "toggle-double"
                Elem.searchBox (URL.albums scope 1 "") query
                Elem.pageDetails album curr timeZone
                Elem.spacer
                Elem.albumTags scope albumTags
            Elem.editPanel albumURL $ do
                Elem.textBoxField  "Title" "title" "edit-title"
                Elem.textAreaField "Tags"  "tags"  "edit-tags"
            Elem.deletePanel albumURL
        Elem.canvas source1 source2

-- | Renders a view for the list of tags as text containing HTML.
tagsView :: Scope -> String -> TagGrouping -> [DetailedTag] -> Text
tagsView scope query grouping tags = render $ do
    let title  = "Tags"
        onload = JS.functionCall "TagsViewModel.register" args
        args   = [ JS.toJSON (scopeName scope)
                 , JS.toJSON query]

        albumsURL = URL.albums scope 1 query
        imagesURL = URL.images scope 1 query
        tagsURL   = URL.tags   scope   query

    Elem.document title onload $ do
        Elem.sideBar $ do
            Elem.actionLink Icon.Book  albumsURL
            Elem.actionLink Icon.Image imagesURL
        Elem.aside $ do
            Elem.infoPanel $ do
                Elem.actions $ do
                    Elem.actionGroup $ do
                        Elem.actionLink Icon.Book  albumsURL
                        Elem.actionLink Icon.Image imagesURL
                Elem.searchBox (tagsURL Nothing) query
                Elem.verticalActionGroup $ do
                    Elem.textLink "By Name"       $ tagsURL (Just (ByName Nothing))
                    Elem.textLink "By Category"   $ tagsURL (Just (ByCategory Nothing))
                    Elem.textLink "Uncategorized" $ tagsURL (Just Uncategorized)
                    Elem.textLink "Recent"        $ tagsURL (Just Recent)
        case grouping of
            ByName     group -> Elem.tagsByName        scope query tags group
            ByCategory group -> Elem.tagsByCategory    scope query tags group
            Uncategorized    -> Elem.uncategorizedTags scope query tags
            Recent           -> Elem.recentTags        scope query tags

----------------------------------------------------------------------- Utility

-- | Renders the given HTML as text.
render :: Html () -> Text
render = LazyText.toStrict . renderText

-- | Returns the first number unless it is greater than or less than the
-- | second number, in which case the value rolls over.
rollOver :: (Ord a, Num a) => a -> a -> a; infixl 3 `rollOver`
rollOver x y
    | x > y     = 1
    | x < 1     = y
    | otherwise = x
