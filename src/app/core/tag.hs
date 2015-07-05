module App.Core.Tag where

import App.Common            ( Tag(..), App, Entity, runDB )
import App.Validation        ( Property(..), Validation, isValidTag )
import App.DataSource.SQLite ( selectTags )

-- | Returns the list of all tag entities.
get :: App [Entity Tag]
get = runDB selectTags

-- | Returns valid if all fields of the given tag are valid; otherwise invalid.
-- | Any validation that requires access to the database is ignored.
validate :: Tag -> Validation
validate (Tag name) = isValidTag (Property "tag" name)
