{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.View where

import qualified App.Core.Album as Album
import qualified App.Paths      as Path

import App.Common       ( Album(..), Image(..), Page(..) )
import App.Expression   ( Expression, parse )
import Control.Monad    ( forM_, unless )
import Data.Monoid      ( mempty )
import Data.Text        ( Text, pack, empty )
import Data.Text.Lazy   ( toStrict )
import Database.Engine  ( Entity(..), ID )
import Lucid.Base       ( ToHtml(..), Html, renderText )
import Lucid.Html5
import Text.Printf      ( FieldFormat(..), PrintfArg(..), formatString, printf )

------------------------------------------------------------------------- Types
    
-- | The type of post displayed on an index page.
data IndexType = Albums | Images

instance Show IndexType where
    show Albums = "albums"
    show Images = "images"

instance PrintfArg IndexType where
    formatArg x format = formatString (show x) format { fmtChar = 's' }

------------------------------------------------------------------------- Views

-- | Renders a page for the given album as text containing HTML.
albumPage :: Entity Album -> Text
albumPage (Entity id album @ Album {..}) =
    render $ document (printf "Album %i" id) [] $ do
        
        unless (null albumTagNames) $
            ul_ [id_ "tags", class_ "listBox"] $ 
                forM_ albumTagNames (li_ . toHtml)
    
        forM_ albumPages $ \(Page _ number _) ->
            let page  = Album.getPage album number
                url   = pack $ printf "/album/%i/%i" id number
                thumb = pack $ maybe "" (Path.getPageThumbnailURL id) page
            
            in a_ [href_ url] $ img_ [src_ thumb]

-- | Renders a page for the given album as text containing HTML.
albumsPage :: String -> Int -> Int -> [Entity Album] -> Text
albumsPage query page total albums = 
    render $ document (printf "Albums (%i)" total) [] $
        div_ [class_ "content"] $ do
        
            div_ [class_ "header"] $ do
                div_ [class_ "left" ] $ searchForm query
                div_ [class_ "right"] $ uploadForm
                
            div_ [class_ "gallery"] $ do
                prevPage Albums page query
                nextPage Albums page query
                forM_ albums albumThumbnail

-- | Renders a page for the given image as text containing HTML.
imagesPage :: String -> Int -> Int -> [Entity Image] -> Text
imagesPage query page total images =
    render $ document (printf "Images (%i)" total) [] $
        div_ [class_ "content"] $ do
        
            div_ [class_ "header"] $ do
                div_ [class_ "left" ] $ searchForm query
                div_ [class_ "right"] $ uploadForm
                
            div_ [class_ "gallery"] $ do
                prevPage Images page query
                nextPage Images page query
                forM_ images imageThumbnail

-- | Renders a page for the given album page as text containing HTML.
pagePage :: ID -> Page -> Text
pagePage id page @ Page {..} =
    render $ document (printf "Album %i - page %i" id pageNumber) [] $ 
        img_ [src_ (pack (Path.getPageURL id page))]

-------------------------------------------------------------------- Components

-- | Returns a link to an album that is displayed as a thumbnail.
albumThumbnail :: Entity Album -> Html ()
albumThumbnail album @ (Entity id _) = a_ [href_ url] $ img_ [src_ thumb]
    
    where url   = pack $ printf "/album/%i" id
          thumb = pack $ Path.getAlbumThumbnailURL album

-- | Returns a link to an image that is displayed as a thumbnail.
imageThumbnail :: Entity Image -> Html ()
imageThumbnail (Entity id image) = a_ [href_ url] $ img_ [src_ thumb]
    
    where url   = pack $ printf "/image/%i" id
          thumb = pack $ Path.getImageThumbnailURL image

-- | Creates an HTML document with the given title, list of javascript import
-- | paths and HTML child as the body.
document :: String -> [Text] -> Html a -> Html a
document title imports f = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        meta_  [content_ "text/html;charset=utf-8",  httpEquiv_ "Content-Type"]
        link_  [rel_ "stylesheet", type_ "text/css", href_ "/static/style.css"]
        forM_ imports $ \x -> 
            script_ [type_ "application/javascript;version=1.7", src_ x] empty
    body_ f

-- | Returns a link to the next page of post results.
nextPage :: IndexType -> Int -> String -> Html ()
nextPage post page query = a_ [href_ url] $ div_ [class_ "thumb"] "next" 
    
    where url = pack $ printf "/%s?page=%i%s" post (page + 1) q
          q   = if null query then "" else "&q=" ++ query

-- | Returns a link to the previous page of post results.
prevPage :: IndexType -> Int -> String -> Html ()
prevPage post page query = if page <= 1 then mempty else link
    
    where link = a_ [href_ url] $ div_ [class_ "thumb"] "previous"
          url  = pack $ printf "/%s/?page=%i%s" post (page - 1) q
          q    = if null query then "" else "&q=" ++ query

-- | Returns a search form for filtering posts.
searchForm :: String -> Html ()
searchForm query = form_ 
    [ name_ "search"
    , action_ "albums"
    , method_ "get" ] $ do
        input_ [type_ "text", name_ "q", value_ query']
        button_ [type_ "submit"] "Search"
    
    where query' = if null query then "" else pack query

-- | Returns an upload form for uploading a new post.
uploadForm :: Html ()
uploadForm = form_ 
    [ name_ "upload"
    , action_ "upload"
    , method_ "post"
    , enctype_ "multipart/form-data" ] $ do
        input_  [type_ "file", name_ "uploadedFile"]
        input_  [type_ "text", name_ "tags", autocomplete_ "off"]
        button_ [type_ "submit"] "Upload"

----------------------------------------------------------------------- Utility

-- | Renders the given HTML as text.
render :: Html a -> Text
render = toStrict . renderText
