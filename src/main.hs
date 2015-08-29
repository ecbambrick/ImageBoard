{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified App.Core.Image as Image

import App.Common                    ( (<$$>), runApplication )
import App.Config                    ( Config(..) )
import App.Expression                ( Expression(..), parse )
import App.Validation                ( Validation(..) )
import Control.Applicative           ( (<$>), (<*>) )
import Control.Monad.Reader          ( asks, local )
import Control.Monad.Trans           ( lift )
import Data.Monoid                   ( mconcat )
import Data.Text                     ( pack )
import App.Template                  ( render, toIndexContext, toImageContext
                                     , toImageSetContext )
import Data.Textual                  ( splitOn )
import Network.Wai.Middleware.Static ( (<|>), addBase, hasPrefix, isNotAbsolute
                                     , noDots, staticPolicy )
import Web.Spock                     ( (<//>), get, html, middleware, text
                                     , post, redirect, root, var )
import Web.Spock.Extended            ( getFile, getParam )

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
    
    -- Upload an image along with its tags.
    post "upload" $ do
        (name, _, path) <- getFile "uploadedFile"
        tags            <- splitOn "," <$> getParam "tags"
        results         <- Image.insert path name tags
        
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
        query   <- getParam "q"
        context <- toIndexContext query <$> Image.getFiltered (parse query)
        results <- render "index" context
        
        html results
    
    -- Renders the image details page for the image with the given ID.
    get ("image" <//> var) $ \id -> do
        image    <- Image.getSingle   id
        previous <- Image.getPrevious id
        next     <- Image.getNext     id
        
        let context = toImageSetContext <$> image <*> previous <*> next
        
        case context of
            Nothing      -> redirect "/"
            Just context -> html =<< render "image" context
