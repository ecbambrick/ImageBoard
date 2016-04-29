{-# LANGUAGE OverloadedStrings #-}

module App.Web.Server where

import qualified App.Core.Album as Album
import qualified App.Core.Image as Image
import qualified App.Core.Scope as Scope
import qualified App.Path       as Path
import qualified App.Web.Route  as Route
import qualified App.Web.View   as View

import App.Config                    ( Config(..) )
import App.Core.Types                ( Album(..), DeletionMode(..), Image(..), Scope(..) )
import App.Expression                ( parse )
import App.FileType                  ( FileType(..), getFileType )
import App.Validation                ( Validation(..) )
import Control.Applicative           ( (<$>), (<*>), pure )
import Control.Monad.Reader          ( ReaderT, asks, liftIO, join )
import Data.Maybe                    ( fromMaybe )
import Data.Monoid                   ( (<>), mconcat )
import Data.Textual                  ( display, intercalate, strip, splitOn )
import Database.Engine               ( Entity(..), fromEntity )
import Network.Wai.Middleware.Static ( addBase, hasPrefix, isNotAbsolute
                                     , noDots, staticPolicy )
import Web.Spock                     ( SpockT, delete, get, html, middleware
                                     , text, post, redirect, renderRoute, root )
import Web.Spock.Extended            ( getFile, optionalParam, notFound, serverError )

-- | The route handling for the web service.
routes :: SpockT (ReaderT Config IO) ()
routes = do
    timeZone    <- asks configTimeZone
    storagePath <- asks configStoragePath
    pageSize    <- asks configPageSize

    -- Enables access to thumbnails and images in the storage path.
    middleware $ staticPolicy $ mconcat
        [ noDots
        , isNotAbsolute
        , hasPrefix Path.getDataPrefix
        , addBase storagePath ]

    -- Enables access to other static content such as JavaScript and CSS files.
    middleware $ staticPolicy $ mconcat
        [ noDots
        , isNotAbsolute
        , hasPrefix Path.getStaticPrefix ]

    -- Redirects to the images page.
    get root $ do
        redirect (Route.images Nothing 1 "")

    -- Upload an image or album.
    post Route.uploadRoute $ \scopeName -> do
        scope           <- Scope.querySingle scopeName
        (name, _, path) <- getFile "uploadedFile"
        title           <- optionalParam "title" ""
        tags            <- splitOn "," <$> optionalParam "tags" ""

        case getFileType path name of
            ArchiveType file -> do
                Album.insert file title tags
                redirect (Route.albums scope 1 "")

            ImageType file -> do
                Image.insert file title tags
                redirect (Route.images scope 1 "")

            InvalidType _ -> do
                serverError "invalid file type"

    -- Renders the albums page with albums that match the query parameter.
    get Route.albumsRoute $ \scopeName -> do
        scope <- Scope.querySingle scopeName
        query <- optionalParam "q" ""
        page  <- optionalParam "page" 1

        case scope of
            Nothing -> do
                notFound

            Just (Scope _ scopeQuery) -> do
                let fullQuery = parse query ++ parse scopeQuery

                count  <- Album.count fullQuery
                albums <- Album.query fullQuery page

                html (View.albumsView scope query page count pageSize albums)

    -- Renders the album details page for the album with the given ID.
    get Route.albumRoute $ \scopeName id -> do
        scope <- Scope.querySingle scopeName
        query <- strip <$> optionalParam "q" ""
        album <- Album.querySingle id

        case (scope, album) of
            (Nothing, _)    -> notFound
            (_, Nothing)    -> notFound
            (_, Just album) -> html (View.albumView scope query timeZone album)

    -- Renders the details page for the album page with the give album ID and
    -- page number.
    get Route.pageRoute $ \scopeName id number -> do
        scope <- Scope.querySingle scopeName
        album <- Album.querySingle id

        let page = flip Album.getPage number . fromEntity =<< album

        case (scope, page) of
            (Nothing, _)   -> notFound
            (_, Nothing)   -> notFound
            (_, Just page) -> html (View.pageView scope id page)

    -- Updates the album with the given id with the given POST data.
    post Route.albumRoute $ \scopeName id -> do
        scope  <- Scope.querySingle scopeName
        entity <- Album.querySingle id

        case (scope, entity) of
            (Nothing, _) -> do
                notFound

            (_, Nothing) -> do
                notFound

            (_, Just (Entity _ album)) -> do
                let originalTitle = albumTitle album
                    originalTags  = intercalate "," (albumTagNames album)

                title   <- optionalParam "title" originalTitle
                tags    <- splitOn "," <$> optionalParam "tags" originalTags
                results <- Album.update (Entity id album { albumTitle    = title
                                                         , albumTagNames = tags })

                case results of
                    Valid     -> redirect (Route.album scope id)
                    Invalid e -> serverError (display e)

    -- Delets the album with the given id.
    delete Route.albumRoute $ \scopeName id -> do
        scope     <- Scope.querySingle scopeName
        permanent <- optionalParam "permanent" False

        case (scope, permanent) of
            (Nothing, _) -> return ()
            (_,   False) -> Album.delete MarkAsDeleted     id
            (_,    True) -> Album.delete PermanentlyDelete id

    -- Renders the images page with images that match the query parameter.
    get Route.imagesRoute $ \scopeName -> do
        scope  <- Scope.querySingle scopeName
        query  <- strip <$> optionalParam "q" ""
        page   <- optionalParam "page" 1

        case scope of
            Nothing -> do
                notFound

            Just (Scope _ scopeQuery) -> do
                let fullQuery = parse query ++ parse scopeQuery

                count  <- Image.count fullQuery
                images <- Image.query fullQuery page

                html (View.imagesView scope query page count pageSize images)

    -- Renders the image details page for the image with the given ID.
    get Route.imageRoute $ \scopeName id -> do
        scope <- Scope.querySingle scopeName
        query <- strip <$> optionalParam "q" ""

        case scope of
            Nothing -> do
                notFound

            Just (Scope _ scopeQuery) -> do
                let fullQuery = parse query ++ parse scopeQuery

                (prev, image, next) <- Image.queryTriple fullQuery id

                let view = View.imageView scope query timeZone <$> prev <*> image <*> next

                case (scope, view) of
                    (Nothing, _)   -> notFound
                    (_, Nothing)   -> notFound
                    (_, Just view) -> html view

    -- Updates the image with the given id with the given POST data.
    post Route.imageRoute $ \scopeName id -> do
        scope  <- Scope.querySingle scopeName
        entity <- Image.querySingle id

        case (scope, entity) of
            (_, Nothing) -> do
                notFound

            (Nothing, _) -> do
                notFound

            (_, Just (Entity _ image)) -> do
                let originalTitle = imageTitle image
                    originalTags  = intercalate "," (imageTagNames image)

                title   <- optionalParam "title" originalTitle
                tags    <- splitOn "," <$> optionalParam "tags" originalTags
                results <- Image.update (Entity id image { imageTitle    = title
                                                         , imageTagNames = tags })

                case results of
                    Valid     -> redirect (Route.image scope id "")
                    Invalid e -> serverError (display e)

    -- Delets the image with the given id.
    delete Route.imageRoute $ \scopeName id -> do
        scope     <- Scope.querySingle scopeName
        permanent <- optionalParam "permanent" False

        case (scope, permanent) of
            (Nothing, _) -> return ()
            (_,   False) -> Image.delete MarkAsDeleted     id
            (_,    True) -> Image.delete PermanentlyDelete id
