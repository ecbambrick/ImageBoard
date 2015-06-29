module Core.Validation where

import Text.Printf  (printf)
import Data.Char    (isAlphaNum, isSpace)
import Data.Monoid  (Monoid, (<>), mempty, mappend)

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

-- | Returns valid if the given property is false; otherwise, invalid.
isFalse :: Property Bool -> Validation
isFalse (Property name True) = Invalid [Error name "True" "expected false"]
isFalse (Property _ False)   = Valid

-- | Returns valid if the given property is positive; otherwise, invalid.
isPositive :: (Show a, Num a, Ord a) => Property a -> Validation
isPositive (Property name x)
    | x > 0     = Valid
    | otherwise = Invalid [Error name (show x) "non-positive number"]

-- | Returns valid if the given property is a valid image file type; otherwise,
-- | invalid.
isValidImageFileType :: Property String -> Validation
isValidImageFileType (Property name x)
    | elem x isValidType = Valid
    | otherwise          = Invalid [Error name x "invalid file type"]
    where isValidType = ["jpg", "jpeg", "png", "gif", "webm"]

-- | Returns valid if the given property is a valid tag name; otherwise, 
-- | invalid.
isValidTag :: Property String -> Validation
isValidTag (Property name x)
    | null x            = Invalid [Error name x "empty"]
    | all isSpace x     = Invalid [Error name x "only whitespace"]
    | all isValidChar x = Valid
    | otherwise         = Invalid [Error name x "invalid characters"]
    where isValidChar x = isAlphaNum x || elem x "._- "

----------------------------------------------------------------------- Utility

-- | Returns whether or not the given validation is Valid.
isValid :: Validation -> Bool
isValid (Invalid _) = False
isValid Valid       = True
