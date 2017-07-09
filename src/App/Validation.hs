module App.Validation
    ( Error(..), Result(..), Validation, printErrors, showError, showErrors
    , assert, reject, whenSuccess, whenFailure
    ) where

import App.Core.Types      ( ID )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Validation     ( Result(..), assert, reject, whenSuccess, whenFailure )

-- | The result of validating.
type Validation = Result [Error]

-- | The list of possible application errors.
data Error = IDNotFound ID
           | TagNotFound String
           | CategoriesNotFound [String]
           | InvalidTagName String
           | InvalidScopeName String
           | InvalidScopeExpression String
           | DuplicateHash String
           | InvalidFileSize Int
           | UnrecognizedFile
           | EmptyAlbum
           deriving (Eq)

-- | Returns a human-readable error message for the given error.
showError :: Error -> String
showError (IDNotFound              x) = "No post with ID " ++ show x ++ " was found"
showError (TagNotFound             x) = "No tag with the name \"" ++ x ++ "\" was found"
showError (CategoriesNotFound      x) = "No categories with the names " ++ show x ++ " were found"
showError (InvalidTagName         []) = "Tag name cannot be empty"
showError (InvalidTagName          x) = "Invalid tag name: " ++ x
showError (InvalidScopeName       []) = "Scope name cannot be empty"
showError (InvalidScopeName        x) = "Invalid scope name: " ++ x
showError (InvalidScopeExpression []) = "Scope expression cannot be empty"
showError (InvalidScopeExpression  x) = "Invalid scope expression: " ++ x
showError (DuplicateHash           x) = "Duplicate hash: " ++ x
showError (InvalidFileSize         x) = "File size must be greater than zero: " ++ show x
showError (UnrecognizedFile         ) = "File is not a recognized format"
showError (EmptyAlbum               ) = "Album cannot be empty"

-- | Returns a human-readable error message for the given list of errors.
showErrors :: [Error] -> String
showErrors [ ] = "No Errors"
showErrors [e] = "Error: " ++ showError e
showErrors  es = "Errors:" ++ concatMap (("\n* " ++) . showError) es

-- | Prints a human-readable error message for the given list of errors.
printErrors :: MonadIO m => [Error] -> m ()
printErrors = liftIO . putStrLn . showErrors
