{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Routing ( index, upload ) where

import qualified Data.Text.Lazy as LazyText
import qualified Core.Image as Image

import Common               ( Image(..), Entity(..), App, (<$$>), fromEntity )
import Control.Applicative  ( (<$>) )
import Core.Validation      ( Validation(..) )
import Data.Data            ( Data, Typeable )
import Data.Textual         ( splitOn )
import Happstack.Extended   ( redirect, renderMustache )
import Happstack.Server     ( ContentType(..), Response, badRequest, lookFile
                            , lookText, ok, toResponse )
import Paths                ( imageURL, templatePath )

-------------------------------------------------------------------------- Data

-- | All data required by the index route.
data IndexData = IndexData { images :: [IndexImage] } deriving (Data, Typeable)

-- | Image data required by the index route.
data IndexImage = IndexImage { path :: FilePath } deriving (Data, Typeable)

---------------------------------------------------------------------- Handlers

-- | Renders an upload form along with the current images as thumbnails.
index :: App Response
index = ok =<< renderMustache (templatePath "index")
           =<< toIndexData <$> Image.get

-- | Persists an uploaded image along with its tags to the file system and 
-- | database. Afterwards, redirects to the index or displays a list of errors.
upload :: App Response
upload = do
    (path, _, _) <- lookFile "uploadedFile"
    tags         <- LazyText.unpack <$$> splitOn "," <$> lookText "tags"
    results      <- Image.insert path tags
    
    case results of
        Valid     -> redirect "/"
        Invalid e -> badRequest $ toResponse $ show e

----------------------------------------------------------------------- Utility

-- | Returns index data for the given list of image entities.
toIndexData :: [Entity Image] -> IndexData
toIndexData images = IndexData $ IndexImage . imageURL . fromEntity <$> images
