{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Scope where

import qualified App.Storage.Database as DB
import qualified App.Validation       as Validation

import App.Control    ( runDB )
import App.Core.Types ( Scope(..), App )
import App.Validation ( Error(..), Validation )
import Control.Monad  ( void )
import Data.Char      ( isAlphaNum )
import Data.Textual   ( trim )

----------------------------------------------------------------------- Queries

-- | Returns the scope with the given name.
querySingle :: String -> App (Maybe Scope)
querySingle name
    | name == scopeName defaultScope = return $ Just defaultScope
    | otherwise                      = runDB  $ DB.selectScope name

---------------------------------------------------------------------- Commands

-- | Deletes the scope with the given name.
delete :: String -> App ()
delete = runDB . DB.deleteScope

-- | Inserts a new scope into the database using the given name and expression
-- | or updates an existing scope if one with the same name already exists.
insertOrUpdate :: String -> String -> App (Validation ())
insertOrUpdate name expression = runDB $ do
    let result = Scope <$> validateName name
                       <*> pure expression

    Validation.whenSuccess result $ \scope -> do
        existingID <- DB.selectScopeID (scopeName scope)
        case existingID of
            Nothing      -> void $ DB.insertScope scope
            Just scopeID -> void $ DB.updateScope scopeID scope

    return (void result)

-------------------------------------------------------------------- Validation

-- | Returns valid if the given scope is valid; otherwise invalid.
validateName :: String -> Validation String
validateName name = do
    let trimmedName = trim name
        defaultName = scopeName defaultScope

    Validation.assert (length trimmedName > 0)     [InvalidScopeName name]
    Validation.assert (trimmedName /= defaultName) [InvalidScopeName name]
    Validation.assert (all isAlphaNum trimmedName) [InvalidScopeName name]

    return trimmedName

----------------------------------------------------------------------- Utility

-- | The default scope that does not filter anything.
defaultScope :: Scope
defaultScope = Scope "all" ""
