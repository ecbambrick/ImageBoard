﻿{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified App.Core.Image as Image
import qualified App.Core.Album as Album
import qualified App.View       as View

import App.Common                    ( runApplication )
import App.Config                    ( Config(..) )
import App.Expression                ( parse )
import App.FileType                  ( FileType(..), getFileType )
import App.Paths                     ( getDataPath )
import App.Validation                ( Validation(..) )
import Control.Applicative           ( (<$>), (<*>), pure )
import Control.Monad                 ( join )
import Control.Monad.Reader          ( asks )
import App.Template                  ( render )
import App.Template.Image            ( toImagesContext, toImageSetContext )
import App.Template.Album            ( toAlbumsContext, toAlbumContext
                                     , toPageContext )
import Data.Monoid                   ( mconcat )
import Data.Text                     ( pack )
import Data.Textual                  ( splitOn )
import Database.Engine               ( fromEntity )
import Network.Wai.Middleware.Static ( addBase, hasPrefix, isNotAbsolute
                                     , noDots, staticPolicy )
import Web.Spock                     ( (<//>), get, html, middleware, text
                                     , post, redirect, root, var )
import Web.Spock.Extended            ( getFile, optionalParam )

main = runApplication $ do
    storagePath <- asks configStoragePath
    pageSize    <- asks configPageSize
    
    -- Enables access to thumbnails and images in the storage path.
    middleware $ staticPolicy $ mconcat
        [ noDots 
        , isNotAbsolute 
        , hasPrefix getDataPath
        , addBase storagePath ]
        
    -- Enables access to other static content such as javascript and css files.
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
        page    <- optionalParam "page" 1
        query   <- optionalParam "q" ""
        count   <- Album.count (parse query)
        albums  <- Album.query (parse query) page
        
        html (View.albumsPage query page count pageSize albums)
    
    -- Renders the album details page for the album with the given ID.
    get ("album" <//> var) $ \id -> do
        album <- Album.querySingle id
        
        case album of
            Nothing    -> redirect "/"
            Just album -> html (View.albumPage album)
    
    -- Renders the details page for the album page with the give album ID and 
    -- page number.
    get ("album" <//> var <//> var) $ \id number -> do
        album <- Album.querySingle id
        
        let page = flip Album.getPage number . fromEntity =<< album
        
        case page of
            Nothing   -> redirect "/"
            Just page -> html (View.pagePage id page)
    
    -- Renders the images page with images that match the query parameter.
    get "images" $ do
        page     <- optionalParam "page" 1
        query    <- optionalParam "q" ""
        count    <- Image.count (parse query)
        images   <- Image.query (parse query) page
        
        html (View.imagesPage query page count pageSize images)
    
    -- Renders the image details page for the image with the given ID.
    get ("image" <//> var) $ \id -> do
        timeZone            <- asks configTimeZone
        query               <- optionalParam "q" ""
        (prev, image, next) <- Image.queryTriple (parse query) id
        
        let view = View.imagePage query timeZone <$> prev <*> image <*> next
        
        case view of
            Nothing   -> redirect "/"
            Just view -> html view
