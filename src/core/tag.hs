module Core.Tag where

import Common                   (Config(..), Tag(..), Entity(..), App)
import Database.SQLite.Entities (selectTags)

-- | Returns the list of all tag entities.
get :: App [Entity Tag]
get = withDatabase selectTags
