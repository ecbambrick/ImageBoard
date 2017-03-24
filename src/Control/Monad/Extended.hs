module Control.Monad.Extended where

import Data.Function ( fix )

-- | Takes a value and a a function that takes a recurse function and a new
-- | value. The recursion will continue until the recurse function is not
-- | called.
recurseOn :: a -> ((a -> b) -> a -> b) -> b
recurseOn = flip fix
