module Web.Spock.Extended where

import Data.Functor        ( Functor, (<$>) )
import Data.HashMap.Strict ( (!) )
import Data.Text           ( Text, unpack )
import Control.Monad.Trans ( MonadIO )
import Web.Spock           ( ActionT, UploadedFile(..), files, param' )

-- | Get the uploaded file with the given input field name.
getFile :: (Functor m, MonadIO m) => Text -> ActionT m (String, String, String)
getFile key = do
    uploadedFile <- (! key) <$> files 

    let name        = unpack (uf_name uploadedFile)
        contentType = unpack (uf_contentType uploadedFile)
        location    = uf_tempLocation uploadedFile
    
    return (name, contentType, location)

-- | Read a request param. Spock looks in route captures first, then in POST 
-- | variables and at last in GET variables
getParam :: (Functor m, MonadIO m) => Text -> ActionT m String
getParam = fmap unpack . param'
