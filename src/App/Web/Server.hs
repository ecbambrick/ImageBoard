{-# LANGUAGE OverloadedStrings #-}

module App.Web.Server ( routes ) where

import qualified App.Core.Album                as Album
import qualified App.Core.Image                as Image
import qualified App.Core.Post                 as Post
import qualified App.Core.Scope                as Scope
import qualified App.Path                      as Path
import qualified App.Web.Route                 as Route
import qualified App.Web.URL                   as URL
import qualified App.Web.View                  as View
import qualified Data.Aeson                    as JSON
import qualified Data.Text                     as Text
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai.Middleware.Static as Middleware
import qualified Web.Spock                     as Spock

import App.Config                ( Config(..) )
import App.Core.Post             ( PostType(..) )
import App.Core.Types            ( Album(..), DeletionMode(..), Image(..), Scope(..) )
import App.Expression            ( parse )
import App.Validation            ( Error(..), Validation(..) )
import Control.Applicative       ( (<$>), (<*>), (<|>), pure )
import Control.Monad.Reader      ( ReaderT, asks, liftIO, join )
import Control.Monad.Trans       ( MonadIO, lift )
import Control.Monad.Trans.Maybe ( MaybeT(..), runMaybeT )
import Data.HashMap.Strict       ( (!) )
import Data.Maybe                ( fromMaybe )
import Data.Monoid               ( (<>), mconcat )
import Data.Text                 ( Text )
import Data.Textual              ( display, intercalate, strip, splitOn )
import Database.Engine           ( Entity(..), fromEntity )
import Web.PathPieces            ( PathPiece )
import Web.Spock                 ( delete, get, hookAny, html, post, redirect, root )

---------------------------------------------------------------------- Handlers

-- | The route handling for the web service.
routes :: Spock.SpockT (ReaderT Config IO) ()
routes = do
    storagePath <- asks configStoragePath

    -- Enables access to thumbnails and images in the storage path.
    Spock.middleware $ Middleware.staticPolicy $ mconcat
        [ Middleware.noDots
        , Middleware.isNotAbsolute
        , Middleware.hasPrefix Path.getDataPrefix
        , Middleware.addBase storagePath ]

    -- Enables access to other static content such as JavaScript and CSS files.
    Spock.middleware $ Middleware.staticPolicy $ mconcat
        [ Middleware.noDots
        , Middleware.isNotAbsolute
        , Middleware.hasPrefix Path.getStaticPrefix ]

    -- Redirects to the images page.
    get Route.root $ do
        redirect (URL.images Scope.getDefault 1 "")

    -- Upload an image or album.
    post Route.upload $ \scopeName -> do
        scope        <- fromMaybe Scope.getDefault <$> Scope.querySingle scopeName
        (_, _, path) <- getFile "uploadedFile"
        title        <- optionalParam "title" ""
        tags         <- splitOn "," <$> optionalParam "tags" ""
        result       <- Post.insert path title tags

        case result of
            InvalidPost e -> requestError (display e)
            AlbumPost     -> redirect (URL.albums scope 1 "")
            ImagePost     -> redirect (URL.images scope 1 "")

    apiRoutes
    albumRoutes
    imageRoutes

-- API route handlers.
apiRoutes :: Spock.SpockT (ReaderT Config IO) ()
apiRoutes = do

    -- Gets details for the image with the given ID as JSON.
    get Route.apiImage $ \id -> do
        image <- Image.querySingle id
        case image of
            Nothing -> do
                notFound
            Just (Entity _ image) -> do
                Spock.setHeader "Content-Type" "application/json"
                Spock.lazyBytes (JSON.encode image)

-- Album route handlers.
albumRoutes :: Spock.SpockT (ReaderT Config IO) ()
albumRoutes = do
    pageSize <- asks configPageSize
    timeZone <- asks configTimeZone

    -- Renders the albums page with albums that match the query parameter
    -- within the given scope.
    get Route.albums $ \scopeName -> do
        result <- runMaybeT $ do
            scope <- MaybeT $ Scope.querySingle scopeName
            page  <- lift   $ optionalParam "page" 1
            query <- lift   $ strip <$> optionalParam "q" ""

            let fullQuery = parse (query ++ ", " ++ scopeExpression scope)

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

            return (View.pageView scope album number)

        case result of
            Nothing   -> notFound
            Just view -> html view

    -- Updates the album with the given id within the given scope using POST
    -- parameters.
    post Route.album $ \scopeName id -> do
        result <- runMaybeT $ do
            scope            <- MaybeT $ Scope.querySingle scopeName
            (Entity _ album) <- MaybeT $ Album.querySingle id

            let originalTitle = albumTitle album
                originalTags  = intercalate "," (albumTagNames album)

            title  <- lift $ optionalParam "title" originalTitle
            tags   <- lift $ splitOn "," <$> optionalParam "tags" originalTags
            result <- lift $ Album.update (Entity id album { albumTitle    = title
                                                           , albumTagNames = tags })

            return (URL.album scope id, result)

        case result of
            Nothing           -> notFound
            Just (url, Valid) -> redirect url
            Just (_,       e) -> serverError (display e)

    -- Deletes the album with the given id within the given scope.
    delete Route.album $ \scopeName id -> do
        scope     <- Scope.querySingle scopeName
        permanent <- optionalParam "permanent" False

        case (scope, permanent) of
            (Nothing, _) -> return ()
            (_,   False) -> Album.delete MarkAsDeleted     id
            (_,    True) -> Album.delete PermanentlyDelete id

-- Image route handlers.
imageRoutes :: Spock.SpockT (ReaderT Config IO) ()
imageRoutes = do
    pageSize <- asks configPageSize
    timeZone <- asks configTimeZone

    -- Renders the images page with images that match the query parameter
    -- within the given scope.
    get Route.images $ \scopeName -> do
        result <- runMaybeT $ do
            scope     <- MaybeT $ Scope.querySingle scopeName
            page      <- lift   $ optionalParam "page" 1
            query     <- lift   $ strip <$> optionalParam "q" ""

            let fullQuery = parse (query ++ ", " ++ scopeExpression scope)

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

            let fullQuery = parse (scopeExpression scope ++ ", " ++ query)

            (prev, curr, next) <- MaybeT $ Image.queryTriple fullQuery id
            return (View.imageView scope query timeZone prev curr next)

        case result of
            Nothing   -> notFound
            Just view -> html view

    -- Updates the image with the given id within the given scope using POST
    -- parameters.
    post Route.image $ \scopeName id -> do
        result <- runMaybeT $ do
            scope            <- MaybeT $ Scope.querySingle scopeName
            (Entity _ image) <- MaybeT $ Image.querySingle id

            let originalTitle = imageTitle image
                originalTags  = intercalate "," (imageTagNames image)

            query  <- lift $ strip <$> optionalParam "q" ""
            title  <- lift $ optionalParam "title" originalTitle
            tags   <- lift $ splitOn "," <$> optionalParam "tags" originalTags
            result <- lift $ Image.update (Entity id image { imageTitle    = title
                                                           , imageTagNames = tags })

            return (URL.image scope id query, result)

        case result of
            Nothing           -> notFound
            Just (url, Valid) -> redirect url
            Just (_,       e) -> serverError (display e)

    -- Deletes the image with the given id within the given scope.
    delete Route.image $ \scopeName id -> do
        scope     <- Scope.querySingle scopeName
        permanent <- optionalParam "permanent" False

        case (scope, permanent) of
            (Nothing, _) -> return ()
            (_,   False) -> Image.delete MarkAsDeleted     id
            (_,    True) -> Image.delete PermanentlyDelete id

----------------------------------------------------------------------- Utility

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
requestError :: (MonadIO m) => Text -> Spock.ActionT m a
requestError text = do
    Spock.setStatus HTTP.status400
    Spock.text text

-- Return a 500 status.
serverError :: (MonadIO m) => Text -> Spock.ActionT m a
serverError text = do
    Spock.setStatus HTTP.status500
    Spock.text text

-- | Get the uploaded file with the given input field name.
getFile :: (MonadIO m) => Text -> Spock.ActionT m (String, String, String)
getFile key = do
    uploadedFile <- (! key) <$> Spock.files

    let name        = Text.unpack (Spock.uf_name uploadedFile)
        contentType = Text.unpack (Spock.uf_contentType uploadedFile)
        location    = Spock.uf_tempLocation uploadedFile

    return (name, contentType, location)

-- | Returns a request param or a default value if one it could not be found.
optionalParam :: (PathPiece p, MonadIO m) => Text -> p -> Spock.ActionT m p
optionalParam name defaultValue = fromMaybe defaultValue <$> Spock.param name
