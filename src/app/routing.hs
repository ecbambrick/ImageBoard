{-# LANGUAGE DeriveDataTypeable #-}

module App.Routing ( image, index, search, upload ) where

import qualified App.Core.Image as Image

import App.Common           ( Entity(..), Image(..), Tag(..), App, ID, (<$$>)
                            , fromEntity )
import App.Expression       ( parse )
import App.Paths            ( imageURL, templatePath )
import App.Validation       ( Validation(..) )
import Data.Data            ( Data, Typeable )
import Data.Functor         ( (<$>) )
import Data.Maybe           ( listToMaybe )
import Data.Textual         ( splitOn )
import Happstack.Extended   ( redirect, renderMustache )
import Happstack.Server     ( Response, badRequest, look, lookFile, ok
                            , toResponse )

-------------------------------------------------------------------------- Data

-- | The context data required for an image.
data ImageContext = ImageContext
    { identifier :: ID
    , path       :: String
    } deriving (Data, Typeable)
    
-- | The context data required for the index.
data IndexContext = IndexContext
    { query  :: String
    , images :: [ImageContext]
    } deriving (Data, Typeable)

-- | Returns the index context data for the given query and list of images.
toIndexContext :: String -> [Entity Image] -> IndexContext
toIndexContext query images = IndexContext
    { query  = query
    , images = toImageContext <$> images }

-- | Returns the image context data for the given image.
toImageContext :: Entity Image -> ImageContext
toImageContext (Entity id image) = ImageContext
    { identifier = id
    , path       = imageURL image }

---------------------------------------------------------------------- Handlers

-- | Renders the image details page for the image with the given ID.
image :: ID -> App Response
image id = do
    context <- toImageContext <$$> Image.getSingle id
    results <- case context of 
        Nothing      -> badRequest $ toResponse "Invalid ID"
        Just context -> renderMustache (templatePath "image") context
        
    ok results

-- | Renders the index page with all images.
index :: App Response
index = do
    context <- toIndexContext "" <$> Image.getAll
    results <- renderMustache (templatePath "index") context

    ok results

-- | Renders the index page with images that match the GET query parameter.
search :: App Response
search = do
    query   <- look "q"
    context <- toIndexContext query <$> Image.getFiltered (parse query)
    results <- renderMustache (templatePath "index") context
    
    ok results

-- | Persists an uploaded image along with its tags to the file system and 
-- | database. Afterwards, redirects to the index or displays a list of errors.
upload :: App Response
upload = do
    (path, _, _) <- lookFile "uploadedFile"
    tags         <- splitOn "," <$> look "tags"
    results      <- Image.insert path tags
    
    case results of
        Valid     -> redirect "/"
        Invalid e -> badRequest $ toResponse $ show e
