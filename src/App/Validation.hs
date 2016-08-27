module App.Validation where

import Data.Char    ( isAlphaNum, isSpace )
import Data.Int     ( Int64 )
import Data.List    ( intercalate )
import Data.Monoid  ( Monoid, mempty, mappend )

------------------------------------------------------------------------- Types

-- | The list of possible errors.
data Error = IDNotFound Int64
           | InvalidTag String
           | InvalidScopeName String
           | InvalidScopeExpression String
           | DuplicateHash String
           | InvalidFileSize Int
           | InvalidFileType String

-- | The results of a validation. When invalid, it contains a list of errors.
data Validation = Valid | Invalid [Error]

instance Show Error where
    show (IDNotFound              x) = "No item with ID " ++ show x ++ " was found"
    show (InvalidTag             []) = "Tag name cannot be empty"
    show (InvalidTag              x) = "Invalid tag name: " ++ x
    show (InvalidScopeName       []) = "Scope name cannot be empty"
    show (InvalidScopeName        x) = "Invalid scope name: " ++ x
    show (InvalidScopeExpression []) = "Scope expression cannot be empty"
    show (InvalidScopeExpression  x) = "Invalid scope expression: " ++ x
    show (DuplicateHash           x) = "Duplicate hash: " ++ x
    show (InvalidFileSize         x) = "File size must be greater than zero: " ++ show x
    show (InvalidFileType        []) = "File type cannot be empty"
    show (InvalidFileType         x) = "Invalid file type: " ++ x

instance Show Validation where
    show Valid        = "Valid"
    show (Invalid es) = "Errors:" ++ concatMap (\x -> "\n* " ++ show x) es

instance Monoid Validation where
    mempty                          = Valid
    mappend Valid b                 = b
    mappend a Valid                 = a
    mappend (Invalid a) (Invalid b) = Invalid (a ++ b)

-------------------------------------------------------------------- Validation

-- | Returns whether or not the given validation result is valid.
isValid :: Validation -> Bool
isValid Valid       = True
isValid (Invalid _) = False

-- | Concatonates the given list of validation results. Returns valid if all
-- | elements are valid; otherwise, returns invalid with the list of all errors.
validate :: [Validation] -> Validation
validate = mconcat

-- | Concatonates the given list of validation results. Returns valid if all
-- | elements are valid; otherwise, returns invalid with only the first error.
validateSingle :: [Validation] -> Validation
validateSingle []            = Valid
validateSingle (Invalid x:_) = Invalid x
validateSingle (Valid:xs)    = validateSingle xs

-- | Returns valid if the given value is true; otherwise, returns invalid with
-- | the given error.
verify :: Bool -> Error -> Validation
verify True  _   = Valid
verify False err = Invalid [err]
