module App.Validation where

import Data.Char    ( isAlphaNum, isSpace )
import Data.Monoid  ( Monoid, (<>), mempty, mappend )

------------------------------------------------------------------------- Types

-- | A validation error indicating the name, value, and message of an invalid
-- | property.
data Error = Error String String String
    deriving (Show)

-- | A property with a name and a value to validate.
data Property a = Property String a
    deriving (Show)

-- | The results of a validation, indicating whether the validation was
-- | successful or a list of errors, otherwise.
data Validation = Valid | Invalid [Error]
    deriving (Show)

instance Monoid Validation where
    mempty                          = Valid
    mappend Valid b                 = b
    mappend a Valid                 = a
    mappend (Invalid a) (Invalid b) = Invalid (a ++ b)

-------------------------------------------------------------------- Validation

-- | Returns valid if the given property is positive; otherwise, invalid.
isPositive :: (Show a, Num a, Ord a) => Property a -> Validation
isPositive (Property name x)
    | x <= 0    = Invalid [Error name (show x) "non-positive number"]
    | otherwise = Valid

-- | Returns valid if the given property is a valid tag name; otherwise,
-- | invalid.
isValidTag :: Property String -> Validation
isValidTag (Property name x)
    | null x             = Invalid [Error name x "empty"]
    | head x == '-'      = Invalid [Error name x "begins with '-'"]
    | all isSpace x      = Invalid [Error name x "only whitespace"]
    | any isInvalidTag x = Invalid [Error name x "invalid characters"]
    | otherwise          = Valid
    where isInvalidTag x = not (isAlphaNum x || elem x ".- ")

----------------------------------------------------------------------- Utility

-- | Returns whether or not the given validation is Valid.
isValid :: Validation -> Bool
isValid (Invalid _) = False
isValid Valid       = True

-- | Returns valid if the given value is true; otherwise, returns invalid with
-- | the given error.
verify :: Bool -> Error -> Validation
verify x error = if x then Valid else Invalid [error]
