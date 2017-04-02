module App.Core.Title where

import App.Validation  ( Error, Validation )
import Data.Textual    ( trim )

-------------------------------------------------------------------- Validation

-- | Returns a trimmed version of the string if valid; otherwise, an error.
validate :: String -> Validation String
validate = return . trim
