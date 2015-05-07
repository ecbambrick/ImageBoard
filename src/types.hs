module Types where

import Control.Applicative ((<$>), (<*>))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Database.SQLite.Simple.FromRow

-- | The primary key of an entity.
type ID = Int64

-- | A tag can be attached to an image as additional meta data. An ID of 
-- | nothing indicates a tag that has not yet been persisted.
data Tag = Tag 
    { _tagID :: Maybe ID
    , _tagName :: String }

instance FromRow Tag where 
    fromRow = Tag <$> field <*> field

instance Show Tag where
    show (Tag id name) = printf "(%i,%s)" (fromMaybe 0 id) name

-- | An Image contains the meta data of an image file that has been uploaded. 
-- | An ID of nothing indicates an image that has not yet been persisted.
data Image = Image
    { _imageID :: Maybe ID
    , _imageTitle :: String
    , _imageExtension :: String
    , _imageHash :: String
    , _imageTags :: [Tag] }

instance FromRow Image where 
    fromRow = toImage [] <$> field <*> field <*> field <*> field
        where
            toImage tags id title ext hash = Image id title ext hash tags
            
instance Show Image where
    show (Image id title _ _ tags) = printf "(%i,%s,%s)" (fromMaybe 0 id) title (show tags)
