module Text.JavaScript where

import qualified Data.Aeson              as Aeson
import qualified Data.Text.Lazy          as LazyText
import qualified Data.Text.Lazy.Encoding as Encoding

import Data.Text  ( Text )
import Data.Aeson ( ToJSON )

-- | Converts the given JSON-encodable data into an escaped JavaScript string.
escape :: (ToJSON a) => a -> Text
escape = LazyText.toStrict . Encoding.decodeUtf8 . Aeson.encode
