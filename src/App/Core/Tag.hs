{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Tag where

import App.Common       ( Tag(..), App, runDB )
import App.Validation   ( Property(..), Validation, isValidTag )
import App.Database     ( selectTags )
import Data.List        ( nub )
import Data.Textual     ( strip, toLower )
import Database.Engine  ( Entity )

-------------------------------------------------------------------------- CRUD

-- | Returns the list of all tag entities.
get :: App [Entity Tag]
get = runDB selectTags

----------------------------------------------------------------------- Utility

-- | Returns valid if all fields of the given tag are valid; otherwise invalid.
-- | Any validation that requires access to the database is ignored.
validate :: Tag -> Validation
validate (Tag name) = isValidTag (Property "tag" name)

-- | Sanitizes the given list of tag names.
cleanTags :: [String] -> [String]
cleanTags = filter (not . null) . nub . map (toLower . strip)
