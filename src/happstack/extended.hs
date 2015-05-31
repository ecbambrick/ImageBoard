{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Happstack.Extended where

import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.IO               as Text

import Common                   ( App )
import Control.Monad            ( MonadPlus )
import Control.Monad.Trans      ( MonadIO, liftIO )
import Data.Functor             ( (<$>) )
import Data.Textual             ( splitOn )
import Data.Data                ( Data )
import Happstack.Server         ( Method(..), FilterMonad, Response
                                , ServerMonad, dirs, method, nullDir
                                , toResponseBS )
import Text.Hastache            ( encodeStr, hastacheStr, defaultConfig )
import Text.Hastache.Context    ( mkGenericContext )

-- | A route to guard against when routing requests.
data Route = Root | Any | Path String

-- | Guards against the GET method.
get :: (ServerMonad m, MonadPlus m) => m a -> m a
get = (>>) (method GET)

-- | Guards against the POST method.
post :: (ServerMonad m, MonadPlus m) => m a -> m a
post = (>>) (method POST)

-- | Renders the given byte string as html.
render :: LazyChar8.ByteString -> Response
render = toResponseBS "text/html"

-- | Renders the file from the given file path as html.
renderFile :: (MonadIO m, FilterMonad Response m) => FilePath -> m Response
renderFile filePath = do
    html <- liftIO $ LazyChar8.readFile filePath
    return $ render html

-- | Renders a mustahce template with the given data.
renderMustache :: (MonadIO m, FilterMonad Response m, Data a) => FilePath -> a -> m Response
renderMustache filePath context = do
    template <- liftIO $ Text.readFile filePath
    result   <- liftIO $ hastacheStr defaultConfig template $ mkGenericContext context
    return $ render $ LazyChar8.pack $ LazyText.unpack result

-- | Guard against a route.
url :: (ServerMonad m, MonadPlus m) => Route -> m a -> m a 
url Root        = (>>) nullDir
url Any         = id
url (Path path) = dirs path
