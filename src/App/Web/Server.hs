{-# LANGUAGE OverloadedStrings #-}

module App.Web.Server where

import qualified App.Core.Image as Image
import qualified App.Core.Album as Album
import qualified App.Web.Route  as Route
import qualified App.Web.View   as View

import App.Config                    ( Config(..) )
import App.Core.Types                ( Album(..), DeletionMode(..), Image(..) )
import App.Expression                ( parse )
import App.FileType                  ( FileType(..), getFileType )
import App.Path                      ( getDataPrefix )
import App.Validation                ( Validation(..) )
import Control.Applicative           ( (<$>), (<*>), pure )
import Control.Monad.Reader          ( ReaderT, asks, liftIO, join )
import Data.Monoid                   ( (<>), mconcat )
import Data.Text                     ( pack )
import Data.Textual                  ( display, intercalate, strip, splitOn )
import Database.Engine               ( Entity(..), fromEntity )
import Network.Wai.Middleware.Static ( addBase, hasPrefix, isNotAbsolute
                                     , noDots, staticPolicy )
import Web.Spock                     ( SpockT, delete, get, html, middleware
                                     , text, post, redirect, renderRoute, root )
import Web.Spock.Extended            ( getFile, optionalParam )

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
        , hasPrefix getDataPrefix
        , addBase storagePath ]

    -- Enables access to other static content such as JavaScript and CSS files.
    middleware $ staticPolicy $ mconcat
        [ noDots
        , isNotAbsolute
        , hasPrefix "static" ]

    -- Redirects to the images page.
    get root $ do
        redirect (Route.images 1 "")

    -- Upload an image or album.
    post Route.uploadRoute $ do
        (name, _, path) <- getFile "uploadedFile"
        title           <- optionalParam "title" ""
        tags            <- splitOn "," <$> optionalParam "tags" ""

        case getFileType path name of
            ArchiveType file -> do
                Album.insert file title tags
                redirect (Route.albums 1 "")

            ImageType file -> do
                Image.insert file title tags
                redirect (Route.images 1 "")

            InvalidType _ -> do
                text ("invalid file type")

    -- Renders the albums page with albums that match the query parameter.
    get Route.albumsRoute $ do
        query  <- strip <$> optionalParam "q" ""
        page   <- optionalParam "page" 1
        count  <- Album.count (parse query)
        albums <- Album.query (parse query) page

        html (View.albumsView query page count pageSize albums)

    -- Renders the album details page for the album with the given ID.
    get Route.albumRoute $ \id -> do
        query <- strip <$> optionalParam "q" ""
        album <- Album.querySingle id

        case album of
            Nothing    -> redirect (Route.albums 1 query)
            Just album -> html (View.albumView query timeZone album)

    -- Renders the details page for the album page with the give album ID and
    -- page number.
    get Route.pageRoute $ \id number -> do
        album <- Album.querySingle id

        let page = flip Album.getPage number . fromEntity =<< album

        case page of
            Nothing   -> redirect (Route.album id)
            Just page -> html (View.pageView id page)

    -- Updates the album with the given id with the given POST data.
    post Route.albumRoute $ \id -> do
        entity <- Album.querySingle id

        case entity of
            Nothing               -> text $ pack "bad id"
            Just (Entity _ album) -> do
                let originalTitle = albumTitle album
                    originalTags  = intercalate "," (albumTagNames album)

                title   <- optionalParam "title" originalTitle
                tags    <- splitOn "," <$> optionalParam "tags" originalTags
                results <- Album.update (Entity id album { albumTitle    = title
                                                         , albumTagNames = tags })

                case results of
                    Valid     -> redirect (Route.album id)
                    Invalid e -> text $ pack (show e)

    -- Delets the album with the given id.
    delete Route.albumRoute $ \id -> do
        permanent <- optionalParam "permanent" False

        if permanent
            then Album.delete PermanentlyDelete id
            else Album.delete MarkAsDeleted     id

    -- Renders the images page with images that match the query parameter.
    get Route.imagesRoute $ do
        query  <- strip <$> optionalParam "q" ""
        page   <- optionalParam "page" 1
        count  <- Image.count (parse query)
        images <- Image.query (parse query) page

        html (View.imagesView query page count pageSize images)

    -- Renders the image details page for the image with the given ID.
    get Route.imageRoute $ \id -> do
        query               <- strip <$> optionalParam "q" ""
        (prev, image, next) <- Image.queryTriple (parse query) id

        let view = View.imageView query timeZone <$> prev <*> image <*> next

        case view of
            Nothing   -> redirect (Route.images 1 query)
            Just view -> html view

    -- Updates the image with the given id with the given POST data.
    post Route.imageRoute $ \id -> do
        entity <- Image.querySingle id

        case entity of
            Nothing               -> text $ pack "bad id"
            Just (Entity _ image) -> do
                let originalTitle = imageTitle image
                    originalTags  = intercalate "," (imageTagNames image)

                title   <- optionalParam "title" originalTitle
                tags    <- splitOn "," <$> optionalParam "tags" originalTags
                results <- Image.update (Entity id image { imageTitle    = title
                                                         , imageTagNames = tags })

                case results of
                    Valid     -> redirect (Route.image id "")
                    Invalid e -> text $ pack (show e)

    -- Delets the image with the given id.
    delete Route.imageRoute $ \id -> do
        permanent <- optionalParam "permanent" False

        if permanent
            then Image.delete PermanentlyDelete id
            else Image.delete MarkAsDeleted     id
