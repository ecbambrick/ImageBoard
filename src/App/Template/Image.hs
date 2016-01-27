{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module App.Template.Image where

import App.Common       ( Image(..) )
import App.Path         ( getImageURL, getImageThumbnailURL )
import Data.Data        ( Data, Typeable )
import Database.Engine  ( Entity(..), ID )

-- | The context data required for an image.
data ImageContext = ImageContext
    { identifier :: ID
    , hasTags    :: Bool
    , path       :: String
    , thumb      :: String
    , tagNames   :: [String]
    } deriving (Data, Typeable)

-- | Returns the image context data for the given image.
toImageContext :: Entity Image -> ImageContext
toImageContext (Entity id image) = ImageContext
    { identifier = id
    , path       = getImageURL image
    , thumb      = getImageThumbnailURL image
    , tagNames   = imageTagNames image
    , hasTags    = not $ null $ imageTagNames image }

-- | The context data required for the image page.
data ImageSetContext = ImageSetContext
    { main     :: ImageContext
    , previous :: ImageContext
    , next     :: ImageContext
    , setQuery :: String
    } deriving (Data, Typeable)

-- | Returns the image set context for the given set of three images.
toImageSetContext :: String -> Entity Image -> Entity Image -> Entity Image -> ImageSetContext
toImageSetContext query image1 image2 image3 = ImageSetContext
    { main     = toImageContext image1
    , previous = toImageContext image2
    , next     = toImageContext image3
    , setQuery = query }

-- | The context data required for the index.
data ImagesContext = ImagesContext
    { query        :: String
    , isQuery      :: Bool
    , page         :: Int
    , previousPage :: Int
    , nextPage     :: Int
    , images       :: [ImageContext]
    , total        :: Int
    } deriving (Data, Typeable)

-- | Returns the index context data for the given query and list of images.
toImagesContext :: String -> Int -> Int -> [Entity Image] -> ImagesContext
toImagesContext query page total images = ImagesContext
    { query        = query
    , isQuery      = not (null query)
    , page         = page
    , previousPage = max (page - 1) 1
    , nextPage     = page + 1
    , images       = map toImageContext images
    , total        = total }
