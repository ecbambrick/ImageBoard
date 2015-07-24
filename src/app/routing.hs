{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module App.Routing ( index, search, upload ) where

import qualified Data.Text.Lazy as LazyText
import qualified App.Core.Image as Image

import App.Common           ( Criteria(..), Image(..), Entity(..), App, (<$$>)
                            , fromEntity )
import App.Expression       ( Token(..), parse )
import App.Paths            ( imageURL, templatePath )
import App.Validation       ( Validation(..) )
import Control.Applicative  ( (<$>) )
import Data.Data            ( Data, Typeable )
import Data.Maybe           ( listToMaybe )
import Data.Textual         ( splitOn )
import Happstack.Extended   ( redirect, renderMustache, render )
import Happstack.Server     ( ContentType(..), Response, badRequest, look
                            , lookFile, ok, toResponse )

-------------------------------------------------------------------------- Data

-- | All data required by the index route.
data IndexData = IndexData
    { query :: String
    , images :: [IndexImage]
    } deriving (Data, Typeable)

-- | Image data required by the index route.
data IndexImage = IndexImage 
    { path :: FilePath
    } deriving (Data, Typeable)

---------------------------------------------------------------------- Handlers

-- | Renders the index page with all images.
index :: App Response
index = renderIndex "" All

-- | Renders the index page with images that match the GET query parameter.
search :: App Response
search = do
    query <- look "q"
    
    renderIndex query (Filtered (parse query))

-- | Persists an uploaded image along with its tags to the file system and 
-- | database. Afterwards, redirects to the index or displays a list of errors.
upload :: App Response
upload = do
    (path, _, _) <- lookFile "uploadedFile"
    tags         <- splitOn "," <$> look "tags"
    results      <- Image.insert path tags
    
    case results of
        Valid     -> redirectToIndex
        Invalid e -> badRequest $ toResponse $ show e

----------------------------------------------------------------------- Utility

-- | Returns index data for the given list of image entities.
toIndexImages :: [Entity Image] -> [IndexImage]
toIndexImages images = IndexImage . imageURL . fromEntity <$> images

-- | Renders the index based on the given criteria.
renderIndex :: String -> Criteria -> App Response
renderIndex query criteria = 
    ok =<< renderMustache (templatePath "index")
       =<< IndexData query <$> toIndexImages <$> Image.get criteria

-- | Redirects to the index.
redirectToIndex :: App Response
redirectToIndex = redirect "/"
