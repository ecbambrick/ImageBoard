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
import Lucid.Base          ( Html(..), renderText )

------------------------------------------------------------------------- Views

-- | Renders a view for the given album as text containing HTML.
albumView :: Scope -> String -> TimeZone -> Album -> Text
albumView scope query timeZone album @ Album {..} = render $ do
    let title    = Text.pack albumTitle
        onload   = JS.functionCall "AlbumViewModel.register" args
        args     = [ JS.toJSON (scopeName scope)
                   , JS.toJSON query
                   , JS.toJSON album ]

    let indexURL = URL.albums scope 1 query
        albumURL = URL.album scope albumID

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
                ( URL.page scope albumID pageNumber
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

        tagsURL         = URL.tags   scope query
        previousPageURL = URL.albums scope (page - 1) query
        firstPageURL    = URL.albums scope 1          query
        nextPageURL     = URL.albums scope (page + 1) query
        imagesURL       = URL.images scope 1          query
        firstResultURL  = URL.album  scope <$> firstID

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
                Elem.imageTags scope (imageTagNames currImage)
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

        tagsURL         = URL.tags   scope query
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
pageView :: Scope -> Album -> Int -> Text
pageView scope album @ Album {..} curr = render $ do
    let title  = Text.pack (albumTitle ++ " (" ++ show curr ++ "/" ++ show pages ++ ")")
        onload = JS.functionCall "PageViewModel.register" args
        args   = [ JS.toJSON (scopeName scope)
                 , JS.toJSON albumID
                 , JS.toJSON prev
                 , JS.toJSON next ]

        pages = length albumPages
        prev  = curr - 1 `rollOver` length albumPages
        next  = curr + 1 `rollOver` length albumPages

        indexURL    = URL.album scope albumID
        prevPageURL = URL.page  scope albumID prev
        nextPageURL = URL.page  scope albumID next

        source1  = maybe "" (URL.pageFile albumID) (Album.getPage album curr)
        source2  = maybe "" (URL.pageFile albumID) (Album.getPage album next)

    Elem.document title onload $ do
        Elem.sideBar $ do
            Elem.actionLink Icon.UpArrow   prevPageURL
            Elem.actionLink Icon.Grid      indexURL
            Elem.actionLink Icon.DownArrow nextPageURL
            Elem.separator
            Elem.action Icon.Pause "toggle-double-compact"
        Elem.aside $ do
            Elem.infoPanel $ do
                Elem.actions $ do
                    Elem.actionGroup $ do
                        Elem.actionLink Icon.LeftArrow  prevPageURL
                        Elem.actionLink Icon.Grid       indexURL
                        Elem.actionLink Icon.RightArrow nextPageURL
                    Elem.actionGroup $ do
                        Elem.action Icon.Pause "toggle-double"
                Elem.pageDetails album curr
        Elem.canvas source1 source2

-- | Renders a view for the list of tags as text containing HTML.
tagsView :: Scope -> String -> [DetailedTag] -> Text
tagsView scope query tags = render $ do
    let title  = "Tags"
        onload = JS.functionCall "TagsViewModel.register" args
        args   = [ JS.toJSON (scopeName scope)
                 , JS.toJSON query]

        albumsURL = URL.albums scope 1 query
        imagesURL = URL.images scope 1 query
        tagsURL   = URL.tags   scope   query

        groups             = groupWith getGroupHeader tags
        getGroupHeader tag = let char = head (detailedTagName tag)
                             in if | isNumber char -> "#"
                                   | isAlpha  char -> [toUpper char]
                                   | otherwise     -> "Symbol"

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
                Elem.searchBox tagsURL query
        Elem.tagList $ do
            forM_ groups $ \group -> do
                Elem.tagHeader (getGroupHeader (head group))
                forM_ group $ \tag -> do
                    Elem.tagDetail scope query tag

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
