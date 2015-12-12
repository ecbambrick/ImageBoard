{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.View where

import qualified App.Core.Album  as Album
import qualified App.Paths       as Path
import qualified Network.URI     as URI

import App.Common       ( Album(..), Image(..), Page(..) )
import App.Expression   ( Expression, parse )
import Control.Monad    ( forM_, unless )
import Data.DateTime    ( TimeZone, defaultFormatDate )
import Data.List        ( intercalate )
import Data.Monoid      ( (<>), mconcat, mempty )
import Data.Text        ( Text, pack, empty )
import Data.Text.Lazy   ( toStrict )
import Database.Engine  ( Entity(..), ID )
import Lucid.Base       ( ToHtml(..), Html, renderText )
import Lucid.Html5
import Numeric          ( showFFloat )
import Text.Printf      ( FieldFormat(..), PrintfArg(..), formatString, printf )

------------------------------------------------------------------------- Types

-- | The type of a UI action. Either a href link or an onclick script.
data ActionType = Link String | Script String

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
imagePage :: String -> TimeZone -> Entity Image -> Entity Image -> Entity Image -> Text
imagePage query timeZone (Entity prev _) (Entity id image @ Image {..}) (Entity next _) =
    render' title scripts $ do
        aside_ $ do
            nav_ $ do
                search_ Images query
                actions_ [navigation, editing]
            elem_ "details" $ do
                elem_ "title" (toHtml imageTitle)
                elem_ "meta" $ do
                    toHtml $ intercalate " | " 
                        [ show imageWidth ++ "x" ++ show imageHeight
                        , formatSize imageFileSize
                        , imageExtension ]
                    br_ []
                    toHtml imageHash
                    br_ []
                    toHtml ("uploaded " ++ defaultFormatDate timeZone imageCreated)
            elem_ "tags" $
                forM_ imageTagNames $ \x ->
                    span_ [class_ "tag"] (toHtml x)
        main_ $
            elem_ "image-container" $
                img_ [id_ "image", src_ imageURL]
    
    where title       = "Image " ++ show id
          params      = parameters [("q", query)]
          imageURL    = pack (Path.getImageURL image)
          scripts     = [ script "/static/request.js"
                        , script "/static/image.js"
                        , imageScript prev next query ]
          navigation  = [ ("arrow-left",  Link   (printf "/image/%i%s"      prev params))
                        , ("th-large",    Link   (printf "/images%s"             params))
                        , ("arrow-right", Link   (printf "/image/%i%s"      next params)) ]
          editing     = [ ("trash",       Script (printf "Image.del(%i, '%s')" id query)) ]

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
          params   = parameters [("q", query)]
          gallery_ = mapM_ $ \(Entity id image) ->
            let url   = pack $ printf "/image/%i%s" id params
                thumb = pack $ Path.getImageThumbnailURL image

            in a_ [href_ url] $ img_ [src_ thumb] :: Html ()

-- | Renders a page for the given album page as text containing HTML.
pagePage :: ID -> Page -> Text
pagePage id page @ Page {..} = render title [] (img_ [src_ url])
    
    where title = printf "Album %i - page %i" id pageNumber
          url   = pack (Path.getPageURL id page)

----------------------------------------------------------------------- Headers

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
        link_  [rel_ "stylesheet", href_ "/static/style2.css"]
        link_  [rel_ "stylesheet", href_ "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"]
        mconcat imports
    body_ f

-------------------------------------------------------------------- Components

-- | Returns an HTML element containing links matching the given list of
-- | icon/URL pairs.
actions_ :: [[(String, ActionType)]] -> Html ()
actions_ actionGroups = 
    elem_ "actions" $
        forM_ actionGroups $ \actions ->
            div_ $
                forM_ actions $ \(icon, action) ->
                    let iconClass = pack ("fa fa-" ++ icon ++ " action")
                        link x    = a_ [class_ iconClass, x] mempty :: Html ()
                        
                    in case action of
                        Link url -> link $ href_    (pack url)
                        Script f -> link $ onclick_ (pack f)

-- | Returns a div element with the given ID.
elem_ :: Text -> Html a -> Html a
elem_ id = div_ [id_ id]

-- | Returns an HTML element containing an icon.
glyph_ :: Text -> Html ()
glyph_ name = i_ [class_ ("fa fa-" <> name)] mempty

-- | Returns a link to the next page of post results.
nextPage_ :: IndexType -> Int -> String -> Int -> Int -> Html ()
nextPage_ post page query total pageSize =  
    if page * pageSize >= total 
        then mempty 
        else link
    
    where link   = a_ [href_ url] $ div_ [class_ "thumb"] "next" 
          params = parameters [("page", show (page + 1)), ("q", query)]
          url    = pack $ printf "/%s%s" post params

-- | Returns a link to the previous page of post results.
prevPage_ :: IndexType -> Int -> String -> Html ()
prevPage_ post page query = 
    if page <= 1 
        then mempty 
        else link
    
    where link   = a_ [href_ url] $ div_ [class_ "thumb"] "previous"
          params = parameters [("page", show (page - 1)), ("q", query)]
          url    = pack $ printf "/%s%s" post params

-- | Returns an element containing tags based on the given list of tag names.
tags_ :: [String] -> Html ()
tags_ tagNames = 
    unless (null tagNames) $
        ul_ [id_ "tags", class_ "listBox"] $ 
            forM_ tagNames (li_ . toHtml)

-- | Returns a search form for filtering posts.
search_ :: IndexType -> String -> Html ()
search_ post query = form_ 
    [ id_ "search"
    , action_ (pack $ "/" ++ show post)
    , method_ "get" ] $ do
        input_  [ id_ "search-text", type_ "text", name_ "q", value_ (pack query) ]
        button_ [ id_ "search-action", type_ "submit" ] (glyph_ "search")

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

-- | Formats the given integer as a file size.
formatSize :: Int -> String
formatSize value
    | value <= 10^3 = "1kb"
    | value >= 10^6 = showFFloat (Just 1) (fromIntegral value / 1000000) "mb"
    | otherwise     = showFFloat (Just 0) (fromIntegral value / 1000)    "kb"

-- | Converts the given list of key-value pairs to a set of parameter values.
parameters :: [(String, String)] -> String
parameters params = 
    let esc                = URI.escapeURIString URI.isUnreserved
        encodePair ("", _) = ""
        encodePair (_, "") = ""
        encodePair (k,  v) = esc k ++ "=" ++ esc v
        results            = intercalate "&" $ filter (not.null) $ map encodePair params
        
    in if null results then "" else "?" ++ results

-- | Renders the given HTML body as text, using the given title and imports to
-- | generate a head element.
render :: String -> [Html ()] -> Html a -> Text
render title imports f = toStrict $ renderText $ document title imports f

-- | Renders the given HTML body as text, using the given title and imports to
-- | generate a head element. Uses an alternate style sheet.
render' :: String -> [Html ()] -> Html a -> Text
render' title imports f = toStrict $ renderText $ document' title imports f
