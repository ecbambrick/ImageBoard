{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiWayIf        #-}

module App.Web.Element where

import qualified App.Expression  as Expression
import qualified App.Web.Icon    as Icon
import qualified App.Web.URL     as URL
import qualified Data.DateTime   as DateTime
import qualified Data.Text       as Text
import qualified Numeric         as Numeric
import qualified System.FilePath as FilePath
import qualified Text.JavaScript as JS

-- Bug in firefox prevents default menuitem_ from working.
import Lucid.Html5 hiding ( menuitem_ )

import App.Core.Types     ( Album(..), DetailedTag(..), Image(..), Scope(..), ID )
import App.Web.Icon       ( Icon )
import Control.Monad      ( forM_ )
import Data.Char          ( isAlpha, isNumber, toUpper )
import Data.DateTime      ( DateTimeFormat(..), TimeZone )
import Data.List.Extended ( bundle, groupWith, sortWith )
import Data.Monoid        ( (<>) )
import Data.Text          ( Text )
import Data.Textual       ( display, intercalate, toLower )
import Lucid.Base         ( Html, term, toHtml )

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
            script_ [type_ ecma6, src_ "/static/viewmodel/albums.js" ] Text.empty
            script_ [type_ ecma6, src_ "/static/viewmodel/images.js" ] Text.empty
            script_ [type_ ecma6, src_ "/static/viewmodel/page.js" ] Text.empty
            script_ [type_ ecma6, src_ "/static/viewmodel/tags.js"] Text.empty
            script_ [type_ ecma6] (JS.onDocumentLoad initialize)
        body_ html

----------------------------------------------------------------------- Actions

-- | Returns an HTML element containing UI actions.
actions :: Html a -> Html a
actions = div_ [id_ "actions"]

-- | Returns an HTML element containing a group of actions.
actionGroup :: Html a -> Html a
actionGroup = div_

-- | Returns an HTML element containing a group of actions, displayed
-- | vertically.
verticalActionGroup :: Html a -> Html a
verticalActionGroup = div_ [class_ "vertical-actions"]

-- | Returns an HTML link with the given icon and ID.
action :: Icon -> Text -> Html ()
action icon id =
    let classes = "action fa " <> Icon.render icon
    in a_ [id_ id, href_ "#", class_ classes] mempty

-- | Returns an a HTML link with the given icon and URL.
actionLink :: Icon -> Text -> Html ()
actionLink icon link =
    let classes = "action fa " <> Icon.render icon
    in a_ [href_ link, class_ classes] mempty

-- | Returns an a HTML link with the given icon and URL.
disabledAction :: Icon -> Html ()
disabledAction icon =
    let classes = "disabled action fa " <> Icon.render icon
    in a_ [href_ "#", class_ classes] mempty

-- | Returns an HTML link with the given label and URL.
textLink :: Text -> Text -> Html ()
textLink text link = a_ [href_ link, class_ "text-link"] (toHtml text)

----------------------------------------------------------------- Context Menus

-- | Returns an HTML context menu with the given ID and list of tag/link pairs.
thumbnailMenu :: (String -> Text) -> String -> ID -> [String] -> Html ()
thumbnailMenu buildURL query postID tags =
    let menuitem_   = flip (term "menuitem") mempty
        onclick url = "window.location.href = '" <> buildURL url <> "'; return false;"

    in menu_ [type_ "context", id_ (getMenuID postID)] $ do

        menuitem_ [ label_ "Exclude this Item"
                  , onclick_ (onclick (query ++ ", -id:" ++ show postID)) ]

        menu_ [label_ "Search by Tag"] $
            forM_ tags $ \tag ->
                menuitem_ [ label_ (Text.pack tag)
                          , onclick_ (onclick tag) ]

        menu_ [label_ "Include Tag"] $
            forM_ tags $ \tag ->
                menuitem_ [ label_ (Text.pack tag)
                          , onclick_ (onclick (query ++ ", " ++ tag)) ]

        menu_ [label_ "Exclude Tag"] $
            forM_ tags $ \tag ->
                menuitem_ [ label_ (Text.pack tag)
                          , onclick_ (onclick (query ++ ", -" ++ tag)) ]

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
sideBar = div_ [id_ "side-bar", class_ "hidden"] . div_ [id_ "side-bar-content"]

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

-------------------------------------------------------------------------- Tags

-- | Returns an HTML element containing a detailed list of tags sorted by name.
tagsByName :: Scope -> String -> [DetailedTag] -> Maybe String -> Html ()
tagsByName scope query tags filterGroup =
    let getGroupHeader tag = let char = head (detailedTagName tag)
                             in if | isNumber char -> "#"
                                   | isAlpha  char -> [toUpper char]
                                   | otherwise     -> "Symbol"

        groups = bundle $ do
                    tag   <- tags
                    group <- [getGroupHeader tag]
                    return (group, tag)

    in groupedTags scope query groups filterGroup

-- | Returns an HTML element containing a detailed list of tags sorted by
-- | category.
tagsByCategory :: Scope -> String -> [DetailedTag] -> Maybe String -> Html ()
tagsByCategory scope query tags filterGroup =
    let groups = bundle $ do
                    tag   <- tags
                    group <- detailedTagCategories tag
                    return (group, tag)

     in groupedTags scope query groups filterGroup

-- | Returns an HTML element containing a detailed list of tags where the tags
-- | are organized into named groups.
groupedTags :: Scope -> String -> [(String, [DetailedTag])] -> Maybe String -> Html ()
groupedTags scope query groups filterGroup =
    let filteredGroups = filter (\(x,_) -> maybe True (== toLower x) filterGroup) groups

    in tagList $ do
        case filteredGroups of
            [(_, tags)] -> do
                mapM_ (tagDetail scope query) tags

            groups -> forM_ groups $ \(header, tags) -> do
                tagHeader header
                mapM_ (tagDetail scope query) tags

-- | Returns an HTML element containing a detailed list of uncategorized tags.
uncategorizedTags :: Scope -> String -> [DetailedTag] -> Html ()
uncategorizedTags scope query tags =
    let uncategorizedTags = filter (null.detailedTagCategories) tags

    in tagList $ do
        forM_ uncategorizedTags $ \tag -> do
            tagDetail scope query tag

-- | Returns an HTML element containing a detailed list of the most recent tags.
recentTags :: Scope -> String -> [DetailedTag] -> Html ()
recentTags scope query tags =
    let recentTags = take 100 $ reverse $ sortWith detailedTagCreated tags

    in tagList $ do
        forM_ recentTags $ \tag -> do
            tagDetail scope query tag

-- | Returns an HTML element containing a detailed list of tags.
tagList :: Html () -> Html ()
tagList = div_ [id_ "tag-list"]

-- | Returns an HTML element containing the given string as a tag header.
tagHeader :: String -> Html ()
tagHeader = div_ [class_ "tag-header"] . toHtml

-- | Returns an HTML element containing tag details for the given tag.
tagDetail :: Scope -> String -> DetailedTag -> Html ()
tagDetail scope query DetailedTag {..} = do
    let fullQuery = Expression.combine [detailedTagName, query]

        sampleLink = case detailedTagSample of
            Left  (Image {..}) -> URL.image scope imageID query
            Right (Album {..}) -> URL.album scope albumID

        sampleThumbnail = case detailedTagSample of
            Left  image -> URL.imageThumb image
            Right album -> URL.albumThumb album

        imageAction = case detailedTagImageCount of
            0 -> disabledAction Icon.Image
            _ -> actionLink     Icon.Image $ URL.images scope 1 fullQuery

        albumAction = case detailedTagAlbumCount of
            0 -> disabledAction Icon.Book
            _ -> actionLink     Icon.Book $ URL.albums scope 1 fullQuery

        imageCount = case detailedTagImageCount of
            0 -> span_ [class_ "disabled"] "0 images"
            1 -> "1 image"
            x -> toHtml (show x ++ " images")

        albumCount = case detailedTagAlbumCount of
            0 -> span_ [class_ "disabled"] "0 albums"
            1 -> "1 album"
            x -> toHtml (show x ++ " albums")

    div_ [class_ "tag-item"] $ do
        a_ [href_ sampleLink] $ do
            img_ [src_ sampleThumbnail]
        div_ [class_ "tag-data"] $ do
            span_ [class_ "tag-name"] (toHtml detailedTagName)
            imageCount
            br_ []
            albumCount
            spacer
            actionGroup $ do
                imageAction
                albumAction

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
            toHtml ("uploaded " ++ DateTime.format ShortDate timeZone albumCreated)

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
            toHtml ("uploaded " ++ DateTime.format ShortDate timeZone imageCreated)

-- | Returns an HTML element for displaying page meta data.
pageDetails :: Album -> Int -> Html ()
pageDetails Album {..} page =
    div_ [class_ "details"] $ do
        div_ [class_ "title"] $ do
            toHtml albumTitle
        div_ [class_ "meta"] $ do
            toHtml $ show page ++ " / " ++ show (length albumPages)

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
canvas :: Text -> Text -> Html ()
canvas url1 url2 =
    let image src id = img_   [ id_ id, class_ "image", src_ src ]
        video src id = video_ [ id_ id, class_ "video", src_ src
                              , autoplay_ "", loop_ "", controls_ ""] mempty :: Html ()

    in main_ [id_ "display"] $ do
        div_ [id_ "current-image-container"] $
            if FilePath.takeExtension (Text.unpack url1) == ".webm"
                then video url1 "current-image"
                else image url1 "current-image"
        div_ [id_ "next-image-container", class_ "hidden"] $
            if FilePath.takeExtension (Text.unpack url2) == ".webm"
                then video url2 "next-image"
                else image url2 "next-image"

-- | Returns an HTML element for displaying a grid of thumbnails.
gallery :: [(Text, String)] -> Html ()
gallery items =
    div_ [id_ "gallery"] $
        forM_ items $ \(url, thumbnail) ->
            let style = "background-image: url('" <> thumbnail <> "');"
            in div_ (a_ [href_ url, style_ (Text.pack style)] mempty)

-- | Returns an HTML element for display a grid of image thumbnails.
imageGallery :: Scope -> String -> [Image] -> Html ()
imageGallery scope query images =
    div_ [id_ "gallery2"] $
        forM_ images $ \image @ Image {..} -> do
            let url      = URL.image scope imageID query
                thumb    = URL.imageThumb image
                buildURL = URL.images scope 0

            a_ [contextmenu_ (getMenuID imageID), href_ url] $ do
                img_ [src_ thumb]
                thumbnailMenu buildURL query imageID imageTags

-- | Returns an HTML element for display a grid of album thumbnails.
albumGallery :: Scope -> String -> [Album] -> Html ()
albumGallery scope query albums =
    div_ [id_ "gallery2"] $
        forM_ albums $ \album @ Album {..} -> do
            let url      = URL.album scope albumID
                thumb    = URL.albumThumb album
                buildURL = URL.albums scope 0

            a_ [contextmenu_ (getMenuID albumID), href_ url] $ do
                img_ [src_ thumb]
                thumbnailMenu buildURL query albumID albumTags

-- | Returns an HTML element for displaying a grid of thumbnails.
gallery2 :: [(Text, Text)] -> Html ()
gallery2 items =
    div_ [id_ "gallery2"] $
        forM_ items $ \(url, thumbnail) ->
            a_ [href_ url] $
                img_ [src_ thumbnail]

----------------------------------------------------------------------- Utility

-- | Formats the given integral value as a file size.
formatSize :: (Integral a) => a -> String
formatSize value
    | value <= 10^3 = "1kb"
    | value >= 10^6 = Numeric.showFFloat (Just 1) (fromIntegral value / 1000000) "mb"
    | otherwise     = Numeric.showFFloat (Just 0) (fromIntegral value / 1000)    "kb"

-- | Converts the given post ID to a context menu ID
getMenuID :: ID -> Text
getMenuID postID = "menu-" <> display postID
