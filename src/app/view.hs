{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.View where

import qualified App.Core.Album as Album
import qualified App.Paths      as Path

import App.Common       ( Album(..), Image(..), Page(..) )
import App.Expression   ( Expression, parse )
import Control.Monad    ( forM_, unless )
import Data.List        ( intercalate )
import Data.Monoid      ( mconcat, mempty )
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
    render title [] $ do
        tags_    albumTagNames
        gallery_ albumPages
        
    where title    = "Album " ++ show id
          gallery_ = mapM_ $ \(Page _ number _) ->
            let page  = Album.getPage album number
                url   = pack $ printf "/album/%i/%i" id number
                thumb = pack $ maybe "" (Path.getPageThumbnailURL id) page

            in a_ [href_ url] $ img_ [src_ thumb] :: Html ()

-- | Renders a page for the given album as text containing HTML.
albumsPage :: String -> Int -> Int -> Int -> [Entity Album] -> Text
albumsPage query page total pageSize albums = 
    render title [] $ do
        div_ [class_ "header"] $ do
            div_ [class_ "left" ] $ searchForm_ Albums query
            div_ [class_ "right"] $ uploadForm_            
        div_ [class_ "gallery"] $ do
            prevPage_ Albums page query
            nextPage_ Albums page query total pageSize
            gallery_  albums
    
    where title    = printf "Albums (%i)" total
          gallery_ = mapM_ $ \album @ (Entity id _) ->
            let url   = pack $ printf "/album/%i" id
                thumb = pack $ Path.getAlbumThumbnailURL album
            
            in a_ [href_ url] $ img_ [src_ thumb] :: Html ()

-- | Renders a page for the given image as text containing HTML.
imagePage :: String -> Entity Image -> Entity Image -> Entity Image -> Text
imagePage query (Entity prev _) (Entity id image @ Image {..}) (Entity next _) =
    render' title scripts $ do
        aside_ $ do
            elem_ "details" $ do
                elem_ "title" (toHtml imageTitle)
                elem_ "meta" $ do
                    toHtml $ intercalate " | " 
                        [ show imageWidth ++ "x" ++ show imageHeight
                        , show imageFileSize
                        , imageExtension ]
                    br_ []
                    toHtml imageHash
                    br_ []
                    toHtml ("uploaded " ++ show imageCreated)
            elem_ "tags" $
                forM_ imageTagNames $ \x ->
                    span_ [class_ "tag"] (toHtml x)
        main_ $
            elem_ "image-container" $
                img_ [id_ "image", src_ imageURL]
    
    where title    = "Image " ++ show id
          imageURL = pack (Path.getImageURL image)
          scripts  = [ script "/static/image.js"
                     , imageScript prev next query ]

-- | Renders a page for the given image as text containing HTML.
imagesPage :: String -> Int -> Int -> Int -> [Entity Image] -> Text
imagesPage query page total pageSize images =
    render title [] $ do
        div_ [class_ "header"] $ do
            div_ [class_ "left" ] $ searchForm_ Images query
            div_ [class_ "right"] $ uploadForm_
        div_ [class_ "gallery"] $ do
            prevPage_ Images page query
            nextPage_ Images page query total pageSize
            gallery_  images
                
    where title    = printf "Images (%i)" total
          query'   = if null query then "" else "?q=" ++ query
          gallery_ = mapM_ $ \(Entity id image) ->
            let url   = pack $ printf "/image/%i%s" id query'
                thumb = pack $ Path.getImageThumbnailURL image

            in a_ [href_ url] $ img_ [src_ thumb] :: Html ()

-- | Renders a page for the given album page as text containing HTML.
pagePage :: ID -> Page -> Text
pagePage id page @ Page {..} = render title [] (img_ [src_ url])
    
    where title = printf "Album %i - page %i" id pageNumber
          url   = pack (Path.getPageURL id page)

-------------------------------------------------------------------- Components

-- | Creates an HTML document with the given title, list of javascript import
-- | paths and HTML child as the body.
document :: String -> [Html ()] -> Html a -> Html a
document title imports f = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        meta_  [content_ "text/html;charset=utf-8",  httpEquiv_ "Content-Type"]
        link_  [rel_ "stylesheet", type_ "text/css", href_ "/static/style.css"]
        mconcat imports
    body_ f

-- | Creates an HTML document with the given title, list of javascript import
-- | paths and HTML child as the body. Uses an alternate style sheet.
document' :: String -> [Html ()] -> Html a -> Html a
document' title imports f = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        meta_  [content_ "text/html;charset=utf-8",  httpEquiv_ "Content-Type"]
        link_  [rel_ "stylesheet", type_ "text/css", href_ "/static/style2.css"]
        mconcat imports
    body_ f

-- | Returns a div element with the given ID.
elem_ :: Text -> Html a -> Html a
elem_ id = div_ [id_ id]

-- | Returns a link to the next page of post results.
nextPage_ :: IndexType -> Int -> String -> Int -> Int -> Html ()
nextPage_ post page query total pageSize =  
    if page * pageSize >= total 
        then mempty 
        else link
    
    where link   = a_ [href_ url] $ div_ [class_ "thumb"] "next" 
          url    = pack $ printf "/%s?page=%i%s" post (page + 1) query'
          query' = if null query then "" else "&q=" ++ query

-- | Returns a link to the previous page of post results.
prevPage_ :: IndexType -> Int -> String -> Html ()
prevPage_ post page query = 
    if page <= 1 
        then mempty 
        else link
    
    where link   = a_ [href_ url] $ div_ [class_ "thumb"] "previous"
          url    = pack $ printf "/%s/?page=%i%s" post (page - 1) query'
          query' = if null query then "" else "&q=" ++ query

-- | Returns an element containing tags based on the given list of tag names.
tags_ :: [String] -> Html ()
tags_ tagNames = 
    unless (null tagNames) $
        ul_ [id_ "tags", class_ "listBox"] $ 
            forM_ tagNames (li_ . toHtml)

-- | Returns a search form for filtering posts.
searchForm_ :: IndexType -> String -> Html ()
searchForm_ post query = form_ 
    [ name_ "search"
    , action_ (pack $ show post)
    , method_ "get" ] $ do
        input_ [type_ "text", name_ "q", value_ (pack query)]
        button_ [type_ "submit"] "Search"

-- | Returns an upload form for uploading a new post.
uploadForm_ :: Html ()
uploadForm_ = form_ 
    [ name_ "upload"
    , action_ "upload"
    , method_ "post"
    , enctype_ "multipart/form-data" ] $ do
        input_  [type_ "file", name_ "uploadedFile"]
        input_  [type_ "text", name_ "tags", autocomplete_ "off"]
        button_ [type_ "submit"] "Upload"

-------------------------------------------------------------------- JavaScript

-- | Returns JavaScript that registers an event listener on the window with 
-- | the given name to the given function.
addListener :: String -> String -> String
addListener = printf "window.addEventListener('%s', %s, false);"

-- | Imports the JavaScript file with the given relative path.
script :: Text -> Html ()
script path = script_ [type_ "application/javascript;version=1.7", src_ path] empty

-- | Registers event listeners for the image page.
imageScript :: ID -> ID -> String -> Html ()
imageScript prev next query =
    script_ [type_ "application/javascript;version=1.7"] $ pack $ unlines
        [ addListener "keyup" navigation ]

    where navigation = printf "Image.navigate(%i, %i, '%s')" prev next query

----------------------------------------------------------------------- Utility

-- | Renders the given HTML body as text, using the given title and imports to
-- | generate a head element.
render :: String -> [Html ()] -> Html a -> Text
render title imports f = toStrict $ renderText $ document title imports f

-- | Renders the given HTML body as text, using the given title and imports to
-- | generate a head element. Uses an alternate style sheet.
render' :: String -> [Html ()] -> Html a -> Text
render' title imports f = toStrict $ renderText $ document' title imports f
