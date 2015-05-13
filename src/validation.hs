module Validation where

import Text.Printf  (printf)
import Data.Char    (isAlphaNum, isSpace)
import Types        (Property(..), Validation(..), Error(..))

-- | Returns valid if a property is nothing; otherwise, applies the given
-- | validation function and returns the results.
allowNothing :: (Property a -> Validation) -> Property (Maybe a) -> Validation
allowNothing f (Property _ Nothing)     = Valid
allowNothing f (Property name (Just x)) = f (Property name x)

-- | Returns valid if the given property is at least a given amount; otherwise,
-- | invalid.
atLeast :: (Show a, Ord a, Num a) => a -> Property a -> Validation
atLeast bound (Property name x)
    | x < bound = Invalid [Error name (show x) "too small"]
    | otherwise = Valid

-- | Returns valid if the given property is a valid tag name; otherwise, 
-- | invalid.
isValidTag :: Property String -> Validation
isValidTag (Property name x)
    | and $ map isValid x = Valid
    | otherwise           = Invalid [Error name x "invalid characters"]
    where
        isValid x = isAlphaNum x || elem x "._- "

-- | Returns valid if the given property is not an empty string or a string 
-- | with only whitespace; otherwise, invalid.
isNotEmpty :: Property String -> Validation
isNotEmpty (Property name x)
    | null x              = Invalid [Error name x "empty string"]
    | and $ map isSpace x = Invalid [Error name x "empty string"]
    | otherwise           = Valid
