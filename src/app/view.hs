{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.View where

import qualified App.Core.Album          as Album
import qualified App.Paths               as Path
import qualified Data.Aeson              as Aeson
import qualified Network.URI             as URI
import qualified Data.Text.Lazy.Encoding as Encoding

import App.Common       ( Album(..), Image(..), Page(..) )
import App.Expression   ( Expression, parse )
import Control.Monad    ( forM_, unless )
import Data.DateTime    ( TimeZone, defaultFormatDate )
import Data.Monoid      ( (<>), mconcat, mempty )
import Data.Text        ( Text, pack, empty )
import Data.Text.Lazy   ( toStrict )
import Data.Textual     ( display, intercalate )
import Database.Engine  ( Entity(..), ID )
import Lucid.Base       ( Attribute, ToHtml(..), Html, renderText )
import Lucid.Html5
import Numeric          ( showFFloat )
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
imagePage :: String -> TimeZone -> Entity Image -> Entity Image -> Entity Image -> Text
imagePage query timeZone (Entity prev _) (Entity id image @ Image {..}) (Entity next _) =

    let title   = "Image " <> toHtml (show id)
        params  = parameters [("q", query)]
        args    = [escapeJS prev, escapeJS id, escapeJS next, escapeJS query]
        onload  = "Image.initializePage(" <> intercalate ", " args <> ")"
        
    in render' title onload $ do
        aside_ $ do
            nav_ $ do
                search_ Images query
                elem_ "actions" $ do
                    div_ $ do
                        actionLink_ "arrow-left"  [href_ (pack (printf "/image/%i%s" prev params))]
                        actionLink_ "th-large"    [href_ (pack (printf "/images%s"        params))]
                        actionLink_ "arrow-right" [href_ (pack (printf "/image/%i%s" next params))]
                    div_ $ do
                        actionLink_ "pencil" [id_ "edit-show", href_ "#"]
                        actionLink_ "trash"  [id_ "delete",    href_ "#"]
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
                img_ [id_ "image", src_ (pack (Path.getImageURL image))]
        form_ 
            [ id_ "edit-form" 
            , action_ ("/image/" <> display id)
            , method_ "post" ] $ do
            div_ [class_ "edit-pair"] $ do
                span_ "Title"
                input_ [id_ "edit-title", name_ "title", type_ "text"]
            div_ [class_ "edit-pair"] $ do
                span_ "Tags"
                textarea_ [id_ "edit-tags", name_ "tags", rows_ "4"] mempty
            div_ [id_ "edit-actions"] $ do
                actionLink_ "check" [id_ "edit-submit", href_ "#"]
                actionLink_ "times" [id_ "edit-cancel", href_ "#"]

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

-- | Creates an HTML document with the given title, JavaScript on-load function,
-- | and HTML child as the body. Uses an alternate style sheet.
document' :: Html () -> Text -> Html a -> Html a
document' title onload f = doctypehtml_ $ do
    head_ $ do
        title_  title
        meta_   [content_ "text/html;charset=utf-8",  httpEquiv_ "Content-Type"]
        link_   [rel_ "stylesheet", href_ "/static/style2.css"]
        link_   [rel_ "stylesheet", href_ "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"]
        script_ [type_ "application/javascript;version=1.7", src_ "/static/request.js"] empty
        script_ [type_ "application/javascript;version=1.7", src_ "/static/image.js"]   empty
        script_ [type_ "application/javascript;version=1.7"] $
            "document.addEventListener(\"DOMContentLoaded\", () => " <> onload <> ");"
    body_ f

-------------------------------------------------------------------- Components

-- Returns an a element with the given attributes. Additionally, it will 
-- use the "action" class and be displayed as the icon with the given name.
actionLink_ :: Text -> [Attribute] -> Html ()
actionLink_ icon attributes = a_ (attributes ++ [classes]) mempty
    where classes = class_ ("action fa fa-" <> icon)

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
    let escape             = URI.escapeURIString URI.isUnreserved
        encodePair ("", _) = ""
        encodePair (_, "") = ""
        encodePair (k,  v) = escape k ++ "=" ++ escape v
        results            = intercalate "&" $ filter (not.null) $ map encodePair params
        
    in if null results then "" else "?" ++ results

-- | Renders the given HTML body as text, using the given title and imports to
-- | generate a head element.
render :: String -> [Html ()] -> Html a -> Text
render title imports f = toStrict $ renderText $ document title imports f

-- | Renders the given HTML body as text, using the given title and JavaScript 
-- | on-load function to create a head element.
render' :: Html () -> Text -> Html a -> Text
render' title onload f = toStrict $ renderText $ document' title onload f

-- Converts the given JSON-encodable data into an escaped JavaScript string.
escapeJS :: (Aeson.ToJSON a) => a -> Text
escapeJS = toStrict . Encoding.decodeUtf8 . Aeson.encode 
