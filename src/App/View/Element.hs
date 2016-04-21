{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.View.Element where

import qualified Text.JavaScript as JS

import App.Core.Types ( Album(..), Image(..) )
import Control.Monad  ( forM_ )
import Data.DateTime  ( TimeZone(..), defaultFormatDate )
import Data.Monoid    ( (<>), mempty )
import Data.Text      ( Text, empty, pack )
import Data.Textual   ( display, intercalate )
import Lucid.Base     ( Html, toHtml )
import Lucid.Html5
import Numeric        ( showFFloat )

------------------------------------------------------------------------- Types

-- | The type of post to search by.
data SearchAction = AlbumSearch | ImageSearch

-- | A UI icon.
data Icon = LeftArrow | RightArrow | Grid | Pencil | Trash | Check | Cross

-------------------------------------------------------------------- Components

-- | Returns an HTML document with the given title, initialization function and
-- | body.
document :: Text -> Text -> Html a -> Html a
document title initialize html =
    let ecma6 = "application/javascript;version=1.7"
    in doctypehtml_ $ do
        head_ $ do
            title_  (toHtml title)
            meta_   [content_ "text/html;charset=utf-8",  httpEquiv_ "Content-Type"]
            link_   [rel_ "stylesheet", href_ "/static/style.css"]
            link_   [rel_ "stylesheet", href_ "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"]
            script_ [type_ ecma6, src_ "/static/request.js"] empty
            script_ [type_ ecma6, src_ "/static/album.js"] empty
            script_ [type_ ecma6, src_ "/static/albums.js"] empty
            script_ [type_ ecma6, src_ "/static/image.js"] empty
            script_ [type_ ecma6, src_ "/static/images.js"] empty
            script_ [type_ ecma6] (JS.onDocumentLoad initialize)
        body_ html

-- | Returns an HTML link with the given icon and ID.
action :: Icon -> Text -> Html ()
action icon id =
    let classes = "action fa " <> renderIcon icon
    in a_ [id_ id, href_ "#", class_ classes] mempty

-- | Returns an HTML element for grouping actions.
actionGroup :: Html a -> Html a
actionGroup = div_

-- Returns an a HTML link with the given icon and URL reference.
actionLink :: Icon -> Text -> Html ()
actionLink icon link =
    let classes = "action fa " <> renderIcon icon
    in a_ [href_ link, class_ classes] mempty

-- | Returns an HTML element for containing UI actions.
actions :: Html a -> Html a
actions = div_ [id_ "actions"]

albumDetails :: Album -> TimeZone -> Html ()
albumDetails Album {..} timeZone =
    div_ [id_ "details"] $ do
        div_ [id_ "title"] $ do
            toHtml albumTitle
        div_ [id_ "meta"] $ do
            toHtml $ intercalate " | "
                [ show (length albumPages) ++ " pages"
                , formatSize albumFileSize ]
            br_ []
            toHtml ("uploaded " ++ defaultFormatDate timeZone albumCreated)

-- | Returns an HTML form for editing a post.
editForm :: Text -> Html a -> Html ()
editForm actionURL html =
    div_ [class_ "overlay", id_ "edit-screen"] $
        form_ [id_ "edit-form", action_ actionURL, method_ "post"] $ do
            html
            div_ [id_ "edit-actions"] $ do
                action Check "edit-submit"
                action Cross "edit-cancel"

-- | Returns an HTML element for displaying a grid of thumbnails.
gallery :: [(Text, String)] -> Html ()
gallery items =
    div_ [id_ "gallery"] $
        forM_ items $ \(url, thumbnail) ->
            let style = "background-image: url('" <> thumbnail <> "');"
            in div_ (a_ [href_ url, style_ (pack style)] mempty)

-- | Returns an HTML element for displaying an icon.
glyph :: Text -> Html ()
glyph name = i_ [class_ ("fa fa-" <> name)] mempty

-- | Returns an HTML element for displaying an image.
image :: Text -> Html ()
image url = main_ $ div_ [id_ "image-container"] $ img_ [id_ "image", src_ url]

-- | Returns an HTML element for displaying image meta data.
imageDetails :: Image -> TimeZone -> Html ()
imageDetails Image {..} timeZone =
    div_ [id_ "details"] $ do
        div_ [id_ "title"] $ do
            toHtml imageTitle
        div_ [id_ "meta"] $ do
            toHtml $ intercalate " | "
                [ show imageWidth ++ "x" ++ show imageHeight
                , formatSize imageFileSize
                , imageExtension ]
            br_ []
            toHtml imageHash
            br_ []
            toHtml ("uploaded " ++ defaultFormatDate timeZone imageCreated)

-- | Returns an HTML form for filtering posts.
searchBox :: SearchAction -> String -> Html ()
searchBox action query =
    form_
        [ id_ "search"
        , action_ (renderAction action)
        , method_ "get" ] $ do
            input_  [ id_ "search-text",   type_ "text", name_ "q", value_ (pack query) ]
            button_ [ id_ "search-action", type_ "submit", tabindex_ "-1" ] $
                glyph "search"

-- | Returns an HTML element representing a side panel.
sidePanel :: Html a -> Html a
sidePanel = aside_ . div_ [class_ "panel"]

-- | Returns an HTML elements representing an empty space between other
-- | elements.
spacer :: Html ()
spacer = div_ [class_ "spacer"] mempty

-- | Returns an HTML element containing the given list of tag names.
tags :: [String] -> Html ()
tags tagNames =
    let toElement = span_ [class_ "tag"] . toHtml
    in div_ [id_ "tags"] (forM_ tagNames toElement)

-- | Returns an HTML element for modifying a post property using a text box.
textAreaField :: Text -> Text -> Text -> Html ()
textAreaField label name id =
    div_ [class_ "edit-pair"] $ do
        span_ (toHtml label)
        textarea_ [id_ id, name_ name, rows_ "4"] mempty

-- | Returns an HTML element for modifying a post property using a text area.
textBoxField :: Text -> Text -> Text -> Html ()
textBoxField label name id =
    div_ [class_ "edit-pair"] $ do
        span_ (toHtml label)
        input_ [id_ id, name_ name, type_ "text"]

uploadForm :: Html ()
uploadForm = do
    h1_ "New File"
    form_
        [ id_ "upload"
        , name_ "upload"
        , action_ "/upload"
        , method_ "post"
        , enctype_ "multipart/form-data" ] $ do
            textBoxField "Title" "title" "upload-title"
            textBoxField "Tags"  "tags"  "upload-tags"
            div_ [class_ "browse"] $
                input_  [type_ "file", name_ "uploadedFile"]
            button_ [type_ "submit", class_ "action"] ("Upload " <> glyph "arrow-up")

-- | Returns an HTML element for displaying a video.
video :: Text -> Html ()
video url =
    main_ $ div_ [id_ "image-container"] $
        video_ [id_ "video", src_ url, autoplay_ "", loop_ "", controls_ ""] mempty

----------------------------------------------------------------------- Utility

-- | Formats the given integral value as a file size.
formatSize :: (Integral a) => a -> String
formatSize value
    | value <= 10^3 = "1kb"
    | value >= 10^6 = showFFloat (Just 1) (fromIntegral value / 1000000) "mb"
    | otherwise     = showFFloat (Just 0) (fromIntegral value / 1000)    "kb"

-- | Renders the given form action as a URL.
renderAction :: SearchAction -> Text
renderAction ImageSearch = "/images/"
renderAction AlbumSearch = "/albums/"

-- | Renders the given icon as a CSS class.
renderIcon :: Icon -> Text
renderIcon LeftArrow  = "fa-arrow-left"
renderIcon RightArrow = "fa-arrow-right"
renderIcon Grid       = "fa-th-large"
renderIcon Pencil     = "fa-pencil"
renderIcon Trash      = "fa-trash"
renderIcon Check      = "fa-check"
renderIcon Cross      = "fa-times"
