{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified App.Core.Image as Image

import App.Common                    ( runApplication )
import App.Config                    ( Config(..) )
import App.Expression                ( parse )
import App.Validation                ( Validation(..) )
import Control.Applicative           ( (<$>), (<*>), pure )
import Control.Monad.Reader          ( asks )
import Control.Monad.Trans           ( lift )
import Data.Monoid                   ( mconcat )
import Data.Text                     ( pack )
import App.Template                  ( render, toIndexContext, toImageSetContext )
import Data.Textual                  ( splitOn )
import Network.Wai.Middleware.Static ( (<|>), addBase, hasPrefix, isNotAbsolute
                                     , noDots, staticPolicy )
import System.FilePath               ( takeBaseName )
import Web.Spock                     ( (<//>), get, html, middleware, text
                                     , post, redirect, root, var )
import Web.Spock.Extended            ( getFile, optionalParam )

-- Main.
main :: IO ()
main = runApplication $ do
    storagePath <- lift (asks configStoragePath)
    
    -- Enables access to thumbnails and images in the storage path.
    middleware $ staticPolicy $ mconcat
        [ noDots 
        , isNotAbsolute 
        , hasPrefix "thumbs" <|> hasPrefix "images"
        , addBase storagePath ]
        
    -- Enables access to other static content such as javascript and css files.
    middleware $ staticPolicy $ mconcat
        [ noDots 
        , isNotAbsolute 
        , hasPrefix "static" ]
    
    -- Upload an image along with its tags.
    post "upload" $ do
        (name, _, path) <- getFile "uploadedFile"
        title           <- optionalParam "title" ""
        tags            <- splitOn "," <$> optionalParam "tags" ""
        results         <- Image.insert path name title tags
        
        case results of
            Valid     -> redirect "/"
            Invalid e -> text $ pack (show e)
    
    -- Renders the index page with all images.
    get root $ do
        page    <- optionalParam "page" 0
        context <- toIndexContext "" page <$> Image.query [] page
        results <- render "index" context
        
        html results
    
    -- Renders the index page with images that match the query parameter.
    get "search" $ do
        page    <- optionalParam "page" 0
        query   <- optionalParam "q" ""
        context <- toIndexContext query page <$> Image.query (parse query) page
        results <- render "index" context
        
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
