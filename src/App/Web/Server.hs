{-# LANGUAGE OverloadedStrings #-}

module App.Web.Server ( routes ) where

import qualified App.Config                    as Config
import qualified App.Core.Album                as Album
import qualified App.Core.Image                as Image
import qualified App.Core.Post                 as Post
import qualified App.Core.Scope                as Scope
import qualified App.Core.Tag                  as Tag
import qualified App.Expression                as Expression
import qualified App.Validation                as Validation
import qualified App.Web.Route                 as Route
import qualified App.Web.URL                   as URL
import qualified App.Web.View                  as View
import qualified Data.Text                     as Text
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai.Middleware.Static as Middleware
import qualified Web.Spock                     as Spock

import App.Config                ( Config )
import App.Core.Post             ( PostType(..) )
import App.Core.Types            ( Album(..), DeletionMode(..), Image(..), Scope(..) )
import App.Web.URL               ( TagGrouping(..) )
import App.Validation            ( Error(..), Result(..) )
import Control.Monad.Trans       ( MonadIO, lift, liftIO )
import Control.Monad.Trans.Maybe ( MaybeT(..), runMaybeT )
import Data.HashMap.Strict       ( (!) )
import Data.Maybe                ( fromMaybe )
import Data.Text                 ( Text )
import Data.Textual              ( intercalate, strip, splitOn )
import Web.HttpApiData           ( FromHttpApiData )
import Web.Spock                 ( SpockM, delete, get, html, post, redirect, root )

import qualified System.Directory as Dir
import qualified System.FilePath as FP

---------------------------------------------------------------------- Handlers

-- | The route handling for the web service.
routes :: SpockM () () Config ()
routes = do
    storagePath <- Config.storagePath
    pageSize    <- Config.pageSize
    timeZone    <- Config.timeZone

    -- Enables access to thumbnails and images in the storage path.
    Spock.middleware $ Middleware.staticPolicy $ mconcat
        [ Middleware.noDots
        , Middleware.isNotAbsolute
        , Middleware.hasPrefix URL.dataPrefix
        , Middleware.policy (Just . intercalate "/" . drop 1 . splitOn "/")
        , Middleware.addBase storagePath ]

    -- Enables access to other static content such as JavaScript and CSS files.
    Spock.middleware $ Middleware.staticPolicy $ mconcat
        [ Middleware.noDots
        , Middleware.isNotAbsolute
        , Middleware.hasPrefix URL.staticPrefix ]

    -- Redirects to the images page.
    get Route.root $ do
        redirect (URL.images Scope.defaultScope 1 "")

    -- Upload an image or album.
    post Route.upload $ \scopeName -> do
        scope        <- fromMaybe Scope.defaultScope <$> Scope.querySingle scopeName
        (x, y, path) <- getFile "uploadedFile"
        title        <- optionalParam "title" ""
        tags         <- uncomma <$> optionalParam "tags" ""
        urls         <- uncomma <$> optionalParam "urls" ""
        result       <- Post.insert path title urls tags

        case result of
            InvalidPost e -> requestError (Validation.showErrors e)
            AlbumPost     -> redirect (URL.albums scope 1 "")
            ImagePost     -> redirect (URL.images scope 1 "")

    -- Renders the albums page with albums that match the query parameter
    -- within the given scope.
    get Route.albums $ \scopeName -> do
        result <- runMaybeT $ do
            scope <- MaybeT $ Scope.querySingle scopeName
            page  <- lift   $ optionalParam "page" 1
            query <- lift   $ strip <$> optionalParam "q" ""

            let fullQuery = Expression.parseMany [query, scopeExpression scope]

            count  <- lift $ Album.count fullQuery
            albums <- lift $ Album.query fullQuery page

            return (View.albumsView scope query page count pageSize albums)

        case result of
            Nothing   -> notFound
            Just view -> html view

    -- Renders the album details page for the album with the given ID within
    -- the given scope.
    get Route.album $ \scopeName id -> do
        result <- runMaybeT $ do
            scope <- MaybeT $ Scope.querySingle scopeName
            album <- MaybeT $ Album.querySingle id
            query <- lift   $ strip <$> optionalParam "q" ""

            return (View.albumView scope query timeZone album)

        case result of
            Nothing   -> notFound
            Just view -> html view

    -- Renders the details page for the album page with the give album ID and
    -- page number within the given scope.
    get Route.page $ \scopeName id number -> do
        result <- runMaybeT $ do
            scope <- MaybeT $ Scope.querySingle scopeName
            album <- MaybeT $ Album.querySingle id
            query <- lift   $ strip <$> optionalParam "q" ""

            return (View.pageView scope album timeZone number query)

        case result of
            Nothing   -> notFound
            Just view -> html view

    -- Updates the album with the given id within the given scope using POST
    -- parameters.
    post Route.album $ \scopeName id -> do
        result <- runMaybeT $ do
            scope  <- MaybeT $ Scope.querySingle scopeName
            album  <- MaybeT $ Album.querySingle id
            title  <- lift $ optionalParam "title" (albumTitle album)
            tags   <- lift $ optionalParam "tags"  (comma (albumTags    album))
            urls   <- lift $ optionalParam "urls"  (comma (albumSources album))
            result <- lift $ Album.update id title (uncomma urls) (uncomma tags)

            return (URL.album scope id "", result)

        case result of
            Nothing               -> notFound
            Just (url, Success _) -> redirect url
            Just (_,   Failure e) -> serverError (Validation.showErrors e)

    -- Deletes the album with the given id within the given scope.
    delete Route.album $ \scopeName id -> do
        scope     <- Scope.querySingle scopeName
        permanent <- optionalParam "permanent" False

        case (scope, permanent) of
            (Nothing, _) -> return ()
            (_,   False) -> Album.delete MarkAsDeleted     id
            (_,    True) -> Album.delete PermanentlyDelete id

    -- Renders the images page with images that match the query parameter
    -- within the given scope.
    get Route.images $ \scopeName -> do
        result <- runMaybeT $ do
            scope     <- MaybeT $ Scope.querySingle scopeName
            page      <- lift   $ optionalParam "page" 1
            query     <- lift   $ strip <$> optionalParam "q" ""

            let fullQuery = Expression.parseMany [query, scopeExpression scope]

            count  <- lift $ Image.count fullQuery
            images <- lift $ Image.query fullQuery page

            return (View.imagesView scope query page count pageSize images)

        case result of
            Nothing   -> notFound
            Just view -> html view

    -- Renders the image details page for the image with the given ID within
    -- the given scope.
    get Route.image $ \scopeName id -> do
        result <- runMaybeT $ do
            scope <- MaybeT $ Scope.querySingle scopeName
            query <- lift   $ strip <$> optionalParam "q" ""

            let fullQuery = Expression.parseMany [query, scopeExpression scope]

            (prev, curr, next) <- MaybeT $ Image.queryTriple fullQuery id
            return (View.imageView scope query timeZone prev curr next)

        case result of
            Nothing   -> notFound
            Just view -> html view

    -- Updates the image with the given id within the given scope using POST
    -- parameters.
    post Route.image $ \scopeName id -> do
        result <- runMaybeT $ do
            scope  <- MaybeT $ Scope.querySingle scopeName
            image  <- MaybeT $ Image.querySingle id
            query  <- lift $ optionalParam "q" ""
            title  <- lift $ optionalParam "title" (imageTitle image)
            tags   <- lift $ optionalParam "tags" (comma (imageTags image))
            urls   <- lift $ optionalParam "urls" (comma (imageSources image))
            result <- Image.update id title (uncomma  urls) (uncomma  tags)

            return (URL.image scope id query, result)

        case result of
            Nothing               -> notFound
            Just (url, Success _) -> redirect url
            Just (_,   Failure e) -> serverError (Validation.showErrors e)

    -- Deletes the image with the given id within the given scope.
    delete Route.image $ \scopeName id -> do
        scope     <- Scope.querySingle scopeName
        permanent <- optionalParam "permanent" False

        case (scope, permanent) of
            (Nothing, _) -> return ()
            (_,   False) -> Image.delete MarkAsDeleted     id
            (_,    True) -> Image.delete PermanentlyDelete id

    -- Renders the tag index page which lists all tags.
    get Route.tags $ \scopeName -> do
        result <- runMaybeT $ do
            scope    <- MaybeT $ Scope.querySingle scopeName
            query    <- lift   $ optionalParam "q" ""
            grouping <- lift   $ optionalParam "grouping" (ByName Nothing)
            tags     <- lift   $ Tag.queryDetailed $ Expression.parseMany
                                    [query, scopeExpression scope]

            return (View.tagsView scope query grouping tags)

        case result of
            Nothing   -> notFound
            Just view -> html view

----------------------------------------------------------------------- Utility

-- | Returns a comma-separated string of the elements in the given list.
comma   = intercalate ","

-- | Returns a list of the elements of the given comma-separated string.
uncomma = splitOn ","

-- Return a 404 status.
notFound :: (MonadIO m) => Spock.ActionT m a
notFound = do
    Spock.setStatus HTTP.status404
    Spock.html $ Text.unlines
        [ "<html>"
        , "<head><title>404 - File not found</title></head>"
        , "<body><h1>404 - File not found</h1></body>"
        , "</html>" ]

-- Return a 400 status.
requestError :: (MonadIO m) => String -> Spock.ActionT m a
requestError message = do
    Spock.setStatus HTTP.status400
    Spock.text (Text.pack message)

-- Return a 500 status.
serverError :: (MonadIO m) => String -> Spock.ActionT m a
serverError message = do
    Spock.setStatus HTTP.status500
    Spock.text (Text.pack message)

-- | Get the uploaded file with the given input field name.
getFile :: (MonadIO m) => Text -> Spock.ActionT m (String, String, String)
getFile key = do
    uploadedFile <- (! key) <$> Spock.files

    let name        = Text.unpack (Spock.uf_name uploadedFile)
        contentType = Text.unpack (Spock.uf_contentType uploadedFile)
        location    = Spock.uf_tempLocation uploadedFile

    return (name, contentType, location)

-- | Returns a request param or a default value if one it could not be found.
optionalParam :: (FromHttpApiData p, MonadIO m) => Text -> p -> Spock.ActionT m p
optionalParam name defaultValue = fromMaybe defaultValue <$> Spock.param name
