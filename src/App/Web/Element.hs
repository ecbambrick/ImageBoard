{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Web.Element where

import qualified App.Web.Icon    as Icon
import qualified App.Web.URL     as URL
import qualified Data.Text       as Text
import qualified Numeric         as Numeric
import qualified System.FilePath as FilePath
import qualified Text.JavaScript as JS

import App.Core.Types ( Album(..), Image(..), Scope(..) )
import App.Web.Icon   ( Icon )
import Control.Monad  ( forM_ )
import Data.DateTime  ( TimeZone, defaultFormatDate )
import Data.Monoid    ( (<>), mempty )
import Data.Text      ( Text )
import Data.Textual   ( display, intercalate )
import Lucid.Base     ( Html, toHtml )
import Lucid.Html5

------------------------------------------------------------------------- Types

-- | The action a form button performs.
data ButtonType = Submit | Cancel

-- The type of image an element relates to.
data ImageType = MainImage | SecondaryImage

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
            script_ [type_ ecma6, src_ "https://cdn.jsdelivr.net/kefir/3.1.0/kefir.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/extensions.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/utility.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/session.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/action.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/request.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/url.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/gallery.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/viewmodel/image.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/viewmodel/album.js"] Text.empty
            script_ [type_ ecma6, src_ "/static/albums.js" ] Text.empty
            script_ [type_ ecma6, src_ "/static/viewmodel/images.js" ] Text.empty
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
    let classes = "action fa " <> Icon.render icon
    in a_ [id_ id, href_ "#", class_ classes] mempty

-- Returns an a HTML link with the given icon and URL.
actionLink :: Icon -> Text -> Html ()
actionLink icon link =
    let classes = "action fa " <> Icon.render icon
    in a_ [href_ link, class_ classes] mempty

-- Returns an a HTML link with the given icon and URL.
disabledAction :: Icon -> Html ()
disabledAction icon =
    let classes = "disabled action fa " <> Icon.render icon
    in a_ [href_ "#", class_ classes] mempty

------------------------------------------------------------------------ Panels

-- | Returns an HTML element representing the side menu.
aside :: Html a -> Html a
aside = aside_

-- | Returns an HTML element representing an empty space between other
-- | elements.
spacer :: Html ()
spacer = div_ [class_ "spacer"] mempty

-- | Returns an HTML element representing a horizontal separator.
separator :: Html ()
separator = hr_ mempty

-- | Returns an HTML element representing a side bar.
sideBar :: Html a -> Html a
sideBar = div_ [id_ "side-bar", class_ "hidden"]

-- | Returns an HTML element representing a panel for displaying data.
infoPanel :: Html a -> Html a
infoPanel = div_ [id_ "info-panel"]

-- | Returns an HTML element representing a panel for editing data.
editPanel :: Text -> Html a -> Html ()
editPanel url html =
    form_ [id_ "edit-panel", class_ "hidden", action_ url, method_ "post"] $ do
        h1_ "Edit Post"
        div_ [class_ "error"] mempty
        html
        div_ [class_ "edit-actions"] $ do
            formButton Submit "edit-submit" "Submit" Icon.Check
            formButton Cancel "edit-cancel" "Cancel" Icon.Cross

-- | Returns an HTML element representing a panel for deleting a post.
deletePanel :: Text -> Html ()
deletePanel url =
    div_ [id_ "delete-panel", class_ "hidden"] $ do
        h1_ "Delete Post"
        div_ [class_ "error"] mempty
        label_ [class_ "checkbox-label"] $ do
            input_ [id_ "delete-permanent", type_ "checkbox"]
            span_ "Delete Permanently"
        div_ [class_ "edit-actions"] $ do
            formButton Submit "delete-submit" "Delete" Icon.Check
            formButton Cancel "delete-cancel" "Cancel" Icon.Cross

------------------------------------------------------------------------- Forms

-- | Returns an HTML form for editing a post.
editForm :: Text -> Html a -> Html ()
editForm actionURL html =
    div_ [class_ "overlay", id_ "edit-screen"] $
        form_ [id_ "edit-form", action_ actionURL, method_ "post"] $ do
            html
            div_ [id_ "edit-actions"] $ do
                action Icon.Check "edit-submit"
                action Icon.Cross "edit-cancel"

-- | Returns an HTML form for filtering posts.
searchBox :: Text -> String -> Html ()
searchBox actionURL query =
    form_
        [ id_ "search"
        , action_ actionURL
        , method_ "get" ] $ do
            input_  [ id_ "search-text", type_ "text", name_ "q", value_ (Text.pack query) ]
            formButton Submit "search-submit" "" Icon.Search

-- | Returns an HTML element representing a file upload form.
uploadForm :: Scope -> Html ()
uploadForm scope = do
    h1_ "New File"
    form_
        [ id_ "upload"
        , name_ "upload"
        , action_ (URL.upload scope)
        , method_ "post"
        , enctype_ "multipart/form-data" ] $ do
            textBoxField  "Title" "title"        "upload-title"
            textAreaField "Tags"  "tags"         "upload-tags"
            fileField     "File"  "uploadedFile" "upload-file"
            formButton Submit "upload-submit" "Upload" Icon.UpArrow

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
    let glyph = i_ [class_ ("fa " <> Icon.render icon)] mempty

        buttonType = case button of
            Submit -> [type_ "submit"]
            Cancel -> [type_ "reset"]

        label = if Text.null text
            then glyph
            else toHtml text <> " " <> glyph

    in button_ ([ id_ id, class_ "action"] <> buttonType) label

------------------------------------------------------------------------- Misc.

-- | Returns an HTML element for displaying album meta data.
albumDetails :: Album -> TimeZone -> Html ()
albumDetails Album {..} timeZone =
    div_ [class_ "details"] $ do
        div_ [id_ "title", class_ "title"] $ do
            toHtml albumTitle
        div_ [class_ "meta"] $ do
            toHtml $ intercalate " | "
                [ show (length albumPages) ++ " pages"
                , formatSize albumFileSize ]
            br_ []
            toHtml ("uploaded " ++ defaultFormatDate timeZone albumCreated)

-- | Returns an HTML element for displaying image meta data.
imageDetails :: Image -> TimeZone -> ImageType -> Html ()
imageDetails Image {..} timeZone imageType =
    let classes   = case imageType of
                        MainImage      -> "details"
                        SecondaryImage -> "details hidden"
        titleID   = case imageType of
                        MainImage      -> "current-title"
                        SecondaryImage -> "next-title"
        detailsID = case imageType of
                        MainImage      -> "current-details"
                        SecondaryImage -> "next-details"

    in div_ [id_ detailsID, class_ classes] $ do
        div_ [id_ titleID, class_ "title"] $ do
            toHtml imageTitle
        div_ [class_ "meta"] $ do
            toHtml $ intercalate " | "
                [ show imageWidth ++ "x" ++ show imageHeight
                , formatSize imageFileSize
                , imageExtension ]
            br_ []
            toHtml imageHash
            br_ []
            toHtml ("uploaded " ++ defaultFormatDate timeZone imageCreated)

-- | Returns an HTML element containing the given list of album tag names.
albumTags :: Scope -> [String] -> Html ()
albumTags scope tagNames =
    div_ [id_ "tags"] $ do
        forM_ tagNames $ \name ->
            a_ [class_ "tag", href_ (URL.albums scope 1 name)] (toHtml name)

-- | Returns an HTML element containing the given list of image tag names.
imageTags :: Scope -> [String] -> Html ()
imageTags scope tagNames =
    div_ [id_ "tags"] $ do
        forM_ tagNames $ \name ->
            a_ [class_ "tag", href_ (URL.images scope 1 name)] (toHtml name)

-- | Returns an HTML element for displaying a full size image(s).
display :: String -> String -> Html ()
display url1 url2 =
    let image src id = img_   [ id_ id, class_ "image", src_ (Text.pack src) ]
        video src id = video_ [ id_ id, class_ "video", src_ (Text.pack src)
                              , autoplay_ "", loop_ "", controls_ ""] mempty :: Html ()

    in main_ [id_ "display"] $ do
        div_ [id_ "current-image-container"] $
            if FilePath.takeExtension url1 == ".webm"
                then video url1 "current-image"
                else image url1 "current-image"
        div_ [id_ "next-image-container", class_ "hidden"] $
            if FilePath.takeExtension url2 == ".webm"
                then video url2 "next-image"
                else image url2 "next-image"

-- | Returns an HTML element for displaying a grid of thumbnails.
gallery :: [(Text, String)] -> Html ()
gallery items =
    div_ [id_ "gallery"] $
        forM_ items $ \(url, thumbnail) ->
            let style = "background-image: url('" <> thumbnail <> "');"
            in div_ (a_ [href_ url, style_ (Text.pack style)] mempty)

-- | Returns an HTML element for displaying a grid of thumbnails.
gallery2 :: [(Text, String)] -> Html ()
gallery2 items =
    div_ [id_ "gallery2"] $
        forM_ items $ \(url, thumbnail) ->
            a_ [href_ url] $
                img_ [src_ (Text.pack thumbnail)]

----------------------------------------------------------------------- Utility

-- | Formats the given integral value as a file size.
formatSize :: (Integral a) => a -> String
formatSize value
    | value <= 10^3 = "1kb"
    | value >= 10^6 = Numeric.showFFloat (Just 1) (fromIntegral value / 1000000) "mb"
    | otherwise     = Numeric.showFFloat (Just 0) (fromIntegral value / 1000)    "kb"
