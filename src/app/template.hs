{-# LANGUAGE DeriveDataTypeable #-}

module App.Template where

import qualified Data.Text.IO as Text

import App.Common               ( ID, Entity(..), Image(..) )
import App.Paths                ( imageURL, templatePath, thumbURL )
import Control.Monad.Trans      ( MonadIO, liftIO )
import Data.Data                ( Data, Typeable )
import Data.Text                ( Text )
import Data.Text.Lazy           ( toStrict )
import Text.Hastache            ( encodeStr, hastacheStr, defaultConfig )
import Text.Hastache.Context    ( mkGenericContext )

---------------------------------------------------------------------- Contexts

-- | The context data required for an image.
data ImageContext = ImageContext
    { identifier :: ID
    , path       :: String
    , thumb      :: String
    , tagNames   :: [String]
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
    , images = fmap toImageContext images }

-- | Returns the image context data for the given image.
toImageContext :: Entity Image -> ImageContext
toImageContext (Entity id image) = ImageContext
    { identifier = id
    , path       = imageURL image
    , thumb      = thumbURL image
    , tagNames   = imageTagNames image }

--------------------------------------------------------------------- Rendering

-- | Renders the mustahce template with the given name with the given data.
render :: (MonadIO m, Data a) => FilePath -> a -> m Text
render name context = do
    let genericContext = mkGenericContext context
    template <- liftIO $ Text.readFile (templatePath name)
    result   <- liftIO $ hastacheStr defaultConfig template genericContext
    return (toStrict result)
