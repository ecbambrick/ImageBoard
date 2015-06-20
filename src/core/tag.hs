module Core.Tag where

import Common            ( Tag(..), Entity(..), App, runDB )
import DataSource.SQLite ( selectTags )

-- | Returns the list of all tag entities.
get :: App [Entity Tag]
get = runDB selectTags
