{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Happstack.Extended where

import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.IO               as Text

import Control.Monad            ( MonadPlus )
import Control.Monad.Trans      ( MonadIO, liftIO )
import Data.Data                ( Data )
import Happstack.Server         ( Method(..), FilterMonad, Response
                                , ServerMonad, dirs, method, nullDir
                                , seeOther, toResponse, toResponseBS )
import Text.Hastache            ( encodeStr, hastacheStr, defaultConfig )
import Text.Hastache.Context    ( mkGenericContext )

----------------------------------------------------------------- Method Guards

-- | Guards against the GET method.
get :: (ServerMonad m, MonadPlus m) => m a -> m a
get = (>>) (method GET)

-- | Guards against the POST method.
post :: (ServerMonad m, MonadPlus m) => m a -> m a
post = (>>) (method POST)

------------------------------------------------------------------ Route Guards

-- | Guards against any route.
other :: (ServerMonad m, MonadPlus m) => m a -> m a 
other = id

-- | Guards against an empty route.
root :: (ServerMonad m, MonadPlus m) => m a -> m a 
root = (>>) nullDir

-- | Guards against a route with the specified path.
uri :: (ServerMonad m, MonadPlus m) => String -> m a -> m a 
uri = dirs

--------------------------------------------------------------------- Responses

-- | Respond with 303 See Other and an empty message body.
redirect :: (FilterMonad Response m) => String -> m Response
redirect url = seeOther url $ toResponse LazyText.empty

-- | Renders the given byte string as html.
render :: LazyChar8.ByteString -> Response
render = toResponseBS "text/html"

-- | Renders the file from the given file path as html.
renderFile :: (MonadIO m) => FilePath -> m Response
renderFile filePath = do
    html <- liftIO $ LazyChar8.readFile filePath
    return $ render html

-- | Renders a mustahce template with the given data.
renderMustache :: (MonadIO m, Data a) => FilePath -> a -> m Response
renderMustache filePath context = do
    let genericContext = mkGenericContext context
    template <- liftIO $ Text.readFile filePath
    result   <- liftIO $ hastacheStr defaultConfig template genericContext
    return $ render $ LazyChar8.pack $ LazyText.unpack result
