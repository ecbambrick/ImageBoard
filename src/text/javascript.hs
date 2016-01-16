{-# LANGUAGE OverloadedStrings #-}

module Text.JavaScript 
    ( Aeson.Value, Aeson.toJSON, escape, functionCall, onDocumentLoad ) where

import qualified Data.Aeson              as Aeson
import qualified Data.Text.Lazy          as LazyText
import qualified Data.Text.Lazy.Encoding as Encoding

import Data.Aeson   ( ToJSON, Value, toJSON )
import Data.Monoid  ( (<>) )
import Data.Text    ( Text )
import Data.Textual ( intercalate )

-- | Converts the given JSON-encodable data into an escaped JavaScript string.
escape :: (ToJSON a) => a -> Text
escape = LazyText.toStrict . Encoding.decodeUtf8 . Aeson.encode

-- | Returns text containing a JavaScript function call for a function with 
-- | the given name and arguments.
functionCall :: Text -> [Value] -> Text
functionCall name args = 
    let args' = intercalate ", " (map escape args)
    in name <> "(" <> args' <> ")"

-- | Returns text containing a JavaScript function call that registers the 
-- | given function with the document's load event.
onDocumentLoad :: Text -> Text
onDocumentLoad function = 
    "document.addEventListener(\"DOMContentLoaded\", () => " <> function <> ");"
