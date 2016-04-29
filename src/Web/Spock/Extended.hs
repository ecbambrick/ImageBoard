module Web.Spock.Extended where

import Web.Spock as Spock

import Data.Functor        ( Functor, (<$>) )
import Data.HashMap.Strict ( (!) )
import Data.Maybe          ( fromMaybe )
import Data.Text           ( Text, pack, unpack )
import Control.Monad.Trans ( MonadIO )
import Network.HTTP.Types  ( status404, status500 )
import Web.PathPieces      ( PathPiece )

-- Return a 404 status.
notFound :: (MonadIO m) => ActionCtxT ctx m a
notFound = do
    Spock.setStatus status404
    Spock.html $ pack $ concat
        [ "<html>"
        , "<head><title>404 - File not found</title></head>"
        , "<body><h1>404 - File not found</h1></body>"
        , "</html>" ]

-- Return a 500 status.
serverError :: (MonadIO m) => Text -> ActionCtxT ctx m a
serverError text = do
    Spock.setStatus status500
    Spock.text text

-- | Get the uploaded file with the given input field name.
getFile :: (Functor m, MonadIO m) => Text -> Spock.ActionT m (String, String, String)
getFile key = do
    uploadedFile <- (! key) <$> Spock.files

    let name        = unpack (Spock.uf_name uploadedFile)
        contentType = unpack (Spock.uf_contentType uploadedFile)
        location    = Spock.uf_tempLocation uploadedFile

    return (name, contentType, location)

-- | Returns a request param or a default value if one it could not be found.
optionalParam :: (PathPiece p, Functor m, MonadIO m) => Text -> p -> ActionT m p
optionalParam name defaultValue = fromMaybe defaultValue <$> Spock.param name
