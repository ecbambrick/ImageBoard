{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Control.Applicative ((<$>), (<*>))
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Text.Printf (printf)
import Database.SQLite.Simple.FromRow

-- | A property with a name and a value to validate.
data Property a = Property String a deriving (Show)

-- | A validation error indicating the name, value, and message of an invalid
-- | property.
data Error = Error String String String deriving (Show)

-- | The results of a validation, indicating whether the validation was
-- | successful or a list of errors, otherwise.
data Validation = Valid | Invalid [Error] deriving (Show)

instance Monoid Validation where
    mempty = Valid
    mappend Valid b = b
    mappend a Valid = a
    mappend (Invalid a) (Invalid b) = Invalid (a ++ b)

-- | The primary key of an entity.
type ID = Int64

-- | A tag can be attached to an image as additional meta data. An ID of 
-- | nothing indicates a tag that has not yet been persisted.
data Tag = Tag 
    { _tagID :: Maybe ID
    , _tagName :: String
    } deriving (Data, Typeable)

instance Eq Tag where
    (Tag _ name1) == (Tag _ name2) = name1 == name2

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
    , _imageTags :: [Tag]
    } deriving (Data, Typeable)

instance FromRow Image where 
    fromRow = toImage [] <$> field <*> field <*> field <*> field
        where
            toImage tags id title ext hash = Image id title ext hash tags
            
instance Show Image where
    show (Image id title _ _ tags) = printf "(%i,%s,%s)" (fromMaybe 0 id) title (show tags)
