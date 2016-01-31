{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified App.Core.Image as Image
import qualified App.Core.Album as Album
import qualified App.View       as View

import App.Common                    ( Album(..), DeletionMode(..), Image(..)
                                     , runApplication )
import App.Config                    ( Config(..) )
import App.Expression                ( parse )
import App.FileType                  ( FileType(..), getFileType )
import App.Path                      ( getDataPath )
import App.Validation                ( Validation(..) )
import Control.Applicative           ( (<$>), (<*>), pure )
import Control.Monad                 ( join )
import Control.Monad.Trans           ( liftIO )
import Control.Monad.Reader          ( asks )
import Data.Monoid                   ( (<>), mconcat )
import Data.Text                     ( pack )
import Data.Textual                  ( display, intercalate, splitOn )
import Database.Engine               ( Entity(..), fromEntity )
import Network.Wai.Middleware.Static ( addBase, hasPrefix, isNotAbsolute
                                     , noDots, staticPolicy )
import Web.Spock                     ( (<//>), delete, get, html, middleware
                                     , text, post, redirect, root, var )
import Web.Spock.Extended            ( getFile, optionalParam )

main = runApplication $ do
    timeZone    <- asks configTimeZone
    storagePath <- asks configStoragePath
    pageSize    <- asks configPageSize

    -- Enables access to thumbnails and images in the storage path.
    middleware $ staticPolicy $ mconcat
        [ noDots
        , isNotAbsolute
        , hasPrefix getDataPath
        , addBase storagePath ]

    -- Enables access to other static content such as JavaScript and CSS files.
    middleware $ staticPolicy $ mconcat
        [ noDots
        , isNotAbsolute
        , hasPrefix "static" ]

    -- Upload an image or album.
    post "upload" $ do
        (name, _, path) <- getFile "uploadedFile"
        title           <- optionalParam "title" ""
        tags            <- splitOn "," <$> optionalParam "tags" ""

        results <- case (getFileType path name) of
            ArchiveType file -> Album.insert file title tags
            ImageType   file -> Image.insert file title tags
            InvalidType ""   -> text $ pack ("invalid file type")
            InvalidType ext  -> text $ pack ("invalid file type: " ++ ext)

        case results of
            Valid     -> redirect "/"
            Invalid e -> text $ pack (show e)

    -- Redirects to the images page.
    get root $ do
        redirect "images"

    -- Renders the albums page with albums that match the query parameter.
    get "albums" $ do
        page   <- optionalParam "page" 1
        query  <- optionalParam "q" ""
        count  <- Album.count (parse query)
        albums <- Album.query (parse query) page

        html (View.albumsView query page count pageSize albums)

    -- Renders the album details page for the album with the given ID.
    get ("album" <//> var) $ \id -> do
        query <- optionalParam "q" ""
        album <- Album.querySingle id

        case album of
            Nothing    -> redirect "/"
            Just album -> html (View.albumView query timeZone album)

    -- Renders the details page for the album page with the give album ID and
    -- page number.
    get ("album" <//> var <//> var) $ \id number -> do
        album <- Album.querySingle id

        let page = flip Album.getPage number . fromEntity =<< album

        case page of
            Nothing   -> redirect "/"
            Just page -> html (View.pageView id page)

    -- Updates the album with the given id with the given POST data.
    post ("album" <//> var) $ \id -> do
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
                    Valid     -> redirect ("/album/" <> display id)
                    Invalid e -> text $ pack (show e)

    -- Delets the album with the given id.
    delete ("album" <//> var) $ \id -> do
        permanent <- optionalParam "permanent" False

        if permanent
            then Album.delete PermanentlyDelete id
            else Album.delete MarkAsDeleted     id

    -- Renders the images page with images that match the query parameter.
    get "images" $ do
        page   <- optionalParam "page" 1
        query  <- optionalParam "q" ""
        count  <- Image.count (parse query)
        images <- Image.query (parse query) page

        html (View.imagesView query page count pageSize images)

    -- Renders the image details page for the image with the given ID.
    get ("image" <//> var) $ \id -> do
        query               <- optionalParam "q" ""
        (prev, image, next) <- Image.queryTriple (parse query) id

        let view = View.imageView query timeZone <$> prev <*> image <*> next

        case view of
            Nothing   -> redirect "/"
            Just view -> html view

    -- Updates the image with the given id with the given POST data.
    post ("image" <//> var) $ \id -> do
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
                    Valid     -> redirect ("/image/" <> display id)
                    Invalid e -> text $ pack (show e)

    -- Delets the image with the given id.
    delete ("image" <//> var) $ \id -> do
        permanent <- optionalParam "permanent" False

        if permanent
            then Image.delete PermanentlyDelete id
            else Image.delete MarkAsDeleted     id
