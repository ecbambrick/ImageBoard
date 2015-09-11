{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified App.Core.Image as Image

import App.Common                    ( (<$$>), runApplication )
import App.Config                    ( Config(..) )
import App.Expression                ( Expression(..), parse )
import App.Validation                ( Validation(..) )
import Control.Applicative           ( (<$>), (<*>), pure )
import Control.Monad.Reader          ( asks, local )
import Control.Monad.Trans           ( lift )
import Data.Maybe                    ( fromMaybe )
import Data.Monoid                   ( mconcat )
import Data.Text                     ( pack )
import App.Template                  ( render, toIndexContext, toImageContext
                                     , toImageSetContext )
import Data.Textual                  ( splitOn )
import Network.Wai.Middleware.Static ( (<|>), addBase, hasPrefix, isNotAbsolute
                                     , noDots, staticPolicy )
import System.FilePath               ( takeBaseName )
import Web.Spock                     ( (<//>), get, html, middleware, text
                                     , post, redirect, root, var, param, param' )
import Web.Spock.Extended            ( getFile )

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
        tags            <- splitOn "," <$> param' "tags"
        results         <- Image.insert path name (takeBaseName name) tags
        
        case results of
            Valid     -> redirect "/"
            Invalid e -> text $ pack (show e)
    
    -- Renders the index page with all images.
    get root $ do
        context <- toIndexContext "" <$> Image.getAll
        results <- render "index" context
        
        html results
    
    -- Renders the index page with images that match the query parameter.
    get "search" $ do
        query   <- param' "q"
        context <- toIndexContext query <$> Image.getFiltered (parse query)
        results <- render "index" context
        
        html results
    
    -- Renders the image details page for the image with the given ID.
    get ("image" <//> var) $ \id -> do
        query                   <- fromMaybe "" <$> param "q"
        (previous, image, next) <- Image.getTriple id (parse query)
        
        let context = toImageSetContext <$> pure query <*> image 
                                        <*> previous   <*> next
        
        case context of
            Nothing      -> redirect "/"
            Just context -> html =<< render "image" context
