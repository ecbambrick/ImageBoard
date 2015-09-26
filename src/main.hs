{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified App.Core.Image as Image

import App.Common                    ( runApplication )
import App.Config                    ( Config(..) )
import App.Expression                ( parse )
import App.FileType                  ( FileType(..), getFileType )
import App.Paths                     ( dataPath )
import App.Validation                ( Validation(..) )
import Control.Applicative           ( (<$>), (<*>), pure )
import Control.Monad.Reader          ( asks )
import Data.Monoid                   ( mconcat )
import Data.Text                     ( pack )
import App.Template                  ( render, toImagesContext, toImageSetContext )
import Data.Textual                  ( splitOn )
import Network.Wai.Middleware.Static ( addBase, hasPrefix, isNotAbsolute
                                     , noDots, staticPolicy )
import Web.Spock                     ( (<//>), get, html, middleware, text
                                     , post, redirect, root, var )
import Web.Spock.Extended            ( getFile, optionalParam )

main = runApplication $ do
    storagePath <- asks configStoragePath
    
    -- Enables access to thumbnails and images in the storage path.
    middleware $ staticPolicy $ mconcat
        [ noDots 
        , isNotAbsolute 
        , hasPrefix dataPath
        , addBase storagePath ]
        
    -- Enables access to other static content such as javascript and css files.
    middleware $ staticPolicy $ mconcat
        [ noDots 
        , isNotAbsolute 
        , hasPrefix "static" ]
    
    -- Upload an image.
    post "upload" $ do
        (name, _, path) <- getFile "uploadedFile"
        title           <- optionalParam "title" ""
        tags            <- splitOn "," <$> optionalParam "tags" ""
        
        results <- case (getFileType path name) of
            ImageType   file -> Image.insert file title tags
            InvalidType ""   -> text $ pack ("invalid file type")
            InvalidType ext  -> text $ pack ("invalid file type: " ++ ext)
            
        case results of
            Valid     -> redirect "/"
            Invalid e -> text $ pack (show e)
    
    -- Redirects to the images page.
    get root $ do
        redirect "images"
    
    -- Renders the images page with images that match the query parameter.
    get "images" $ do
        page    <- optionalParam "page" 0
        query   <- optionalParam "q" ""
        context <- toImagesContext query page <$> Image.query (parse query) page
        results <- render "images" context
        
        html results
    
    -- Renders the image details page for the image with the given ID.
    get ("image" <//> var) $ \id -> do
        query                   <- optionalParam "q" ""
        (previous, image, next) <- Image.queryTriple (parse query) id
        
        let context = toImageSetContext <$> pure query <*> image 
                                        <*> previous   <*> next
        
        case context of
            Nothing      -> redirect "/"
            Just context -> html =<< render "image" context
