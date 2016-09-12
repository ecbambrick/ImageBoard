{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Web.Element where

import qualified App.Web.Route   as Route
import qualified Data.Text       as Text
import qualified Numeric         as Numeric
import qualified Text.JavaScript as JS

import App.Core.Types ( Album(..), Image(..), Scope(..) )
import Control.Monad  ( forM_ )
import Data.DateTime  ( TimeZone, defaultFormatDate )
import Data.Monoid    ( (<>), mempty )
import Data.Text      ( Text )
import Data.Textual   ( display, intercalate )
import Lucid.Base     ( Html, toHtml )
import Lucid.Html5

------------------------------------------------------------------------- Types

-- | A UI icon.
data Icon = LeftArrow | RightArrow | UpArrow | Grid | Pencil | Trash | Check
          | Cross | Search

-- | The action a form button performs.
data ButtonType = Submit | Cancel

---------------------------------------------------------------------- Document

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
            script_ [type_ ecma6, src_ "/static/utility.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/request.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/route.js"  ] Text.empty
            script_ [type_ ecma6, src_ "/static/album.js"  ] Text.empty
            script_ [type_ ecma6, src_ "/static/albums.js" ] Text.empty
            script_ [type_ ecma6, src_ "/static/image.js"  ] Text.empty
            script_ [type_ ecma6, src_ "/static/images.js" ] Text.empty
            script_ [type_ ecma6, src_ "/static/page.js"   ] Text.empty
            script_ [type_ ecma6] (JS.onDocumentLoad initialize)
        body_ html

----------------------------------------------------------------------- Actions

-- | Returns an HTML element containing UI actions.
actions :: Html a -> Html a
actions = div_ [id_ "actions"]

-- | Returns an HTML element containing a group of actions.
actionGroup :: Html a -> Html a
actionGroup = div_

-- | Returns an HTML link with the given icon and ID.
action :: Icon -> Text -> Html ()
action icon id =
    let classes = "action fa " <> renderIcon icon
    in a_ [id_ id, href_ "#", class_ classes] mempty

-- Returns an a HTML link with the given icon and URL.
actionLink :: Icon -> Text -> Html ()
actionLink icon link =
    let classes = "action fa " <> renderIcon icon
    in a_ [href_ link, class_ classes] mempty

------------------------------------------------------------------------ Panels

-- | Returns an HTML element representing the side menu.
aside :: Html a -> Html a
aside = aside_

-- | Returns an HTML elements representing an empty space between other
-- | elements.
spacer :: Html ()
spacer = div_ [class_ "spacer"] mempty

-- | Returns an HTML element representing a panel for displaying data.
infoPanel :: Html a -> Html a
infoPanel = div_ [id_ "info-panel"]

-- | Returns an HTML element representing a panel for editing data.
editPanel :: Text -> Html a -> Html ()
editPanel url html = do
    form_ [id_ "edit-panel", action_ url, method_ "post"] $ do
        h1_ "Edit Post"
        div_ [class_ "error"] mempty
        html
        spacer
        div_ [class_ "edit-actions"] $ do
            formButton Submit "edit-submit" "Submit" Check
            formButton Cancel "edit-cancel" "Cancel" Cross

------------------------------------------------------------------------- Forms

-- | Returns an HTML form for editing a post.
editForm :: Text -> Html a -> Html ()
editForm actionURL html =
    div_ [class_ "overlay", id_ "edit-screen"] $
        form_ [id_ "edit-form", action_ actionURL, method_ "post"] $ do
            html
            div_ [id_ "edit-actions"] $ do
                action Check "edit-submit"
                action Cross "edit-cancel"

-- | Returns an HTML form for filtering posts.
searchBox :: Text -> String -> Html ()
searchBox actionURL query =
    form_
        [ id_ "search"
        , action_ actionURL
        , method_ "get" ] $ do
            input_  [ id_ "search-text", type_ "text", name_ "q", value_ (Text.pack query) ]
            formButton Submit "search-submit" "" Search

-- | Returns an HTML element representing a file upload form.
uploadForm :: Maybe Scope -> Html ()
uploadForm scope = do
    h1_ "New File"
    form_
        [ id_ "upload"
        , name_ "upload"
        , action_ (Route.upload scope)
        , method_ "post"
        , enctype_ "multipart/form-data" ] $ do
            textBoxField  "Title" "title"        "upload-title"
            textAreaField "Tags"  "tags"         "upload-tags"
            fileField     "File"  "uploadedFile" "upload-file"
            formButton    Submit "" "Upload" UpArrow

-- | Returns an HTML element for modifying a post property using a text box.
textAreaField :: Text -> Text -> Text -> Html ()
textAreaField label name id =
    div_ [class_ "edit-pair"] $ do
        span_ (toHtml label)
        textarea_ [id_ id, name_ name, rows_ "5"] mempty

-- | Returns an HTML element for modifying a post property using a text area.
textBoxField :: Text -> Text -> Text -> Html ()
textBoxField label name id =
    div_ [class_ "edit-pair"] $ do
        span_ (toHtml label)
        input_ [id_ id, name_ name, type_ "text"]

-- | Returns an HTML element for uploading a file.
fileField :: Text -> Text -> Text -> Html ()
fileField label name id =
    div_ [class_ "edit-pair"] $ do
        span_ (toHtml label)
        div_ [class_ "browse"] $ input_  [id_ id, name_ name, type_ "file"]

-- | Returns an HTML form button
formButton :: ButtonType -> Text -> Text -> Icon -> Html ()
formButton button id text icon =
    let glyph = i_ [class_ ("fa " <> renderIcon icon)] mempty

        buttonType = case button of
            Submit -> [type_ "submit"]
            Cancel -> [type_ "cancel"]

        label = if Text.null text
            then glyph
            else toHtml text <> " " <> glyph

    in button_ ([ id_ id, class_ "action"] <> buttonType) label

------------------------------------------------------------------------- Misc.

-- | Returns an HTML element for displaying album meta data.
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

-- | Returns an HTML element containing the given list of album tag names.
albumTags :: Maybe Scope -> [String] -> Html ()
albumTags scope tagNames =
    div_ [id_ "tags"] $ do
        forM_ tagNames $ \name ->
            a_ [class_ "tag", href_ (Route.albums scope 1 name)] (toHtml name)

-- | Returns an HTML element containing the given list of image tag names.
imageTags :: Maybe Scope -> [String] -> Html ()
imageTags scope tagNames =
    div_ [id_ "tags"] $ do
        forM_ tagNames $ \name ->
            a_ [class_ "tag", href_ (Route.images scope 1 name)] (toHtml name)

-- | Returns an HTML element for displaying a grid of thumbnails.
gallery :: [(Text, String)] -> Html ()
gallery items =
    div_ [id_ "gallery"] $
        forM_ items $ \(url, thumbnail) ->
            let style = "background-image: url('" <> thumbnail <> "');"
            in div_ (a_ [href_ url, style_ (Text.pack style)] mempty)

-- | Returns an HTML element for displaying an image.
image :: Text -> Html ()
image url = main_ $ div_ [id_ "image-container"] $ img_ [id_ "image", src_ url]

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
    | value >= 10^6 = Numeric.showFFloat (Just 1) (fromIntegral value / 1000000) "mb"
    | otherwise     = Numeric.showFFloat (Just 0) (fromIntegral value / 1000)    "kb"

-- | Renders the given icon as a CSS class.
renderIcon :: Icon -> Text
renderIcon icon = case icon of
    LeftArrow  -> "fa-arrow-left"
    RightArrow -> "fa-arrow-right"
    UpArrow    -> "fa-arrow-up"
    Grid       -> "fa-th-large"
    Pencil     -> "fa-pencil"
    Trash      -> "fa-trash"
    Check      -> "fa-check"
    Cross      -> "fa-times"
    Search     -> "fa-search"
