{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Scope ( defaultName, delete, insertOrUpdate, querySingle ) where

import qualified App.Database as DB
import qualified App.Path     as Path

import App.Control           ( runDB )
import App.Core.Types        ( Scope(..), App )
import App.Validation        ( Error(..), Validation, isValid, verify )
import Control.Monad         ( void, when )
import Data.Functor.Extended ( (<$$>) )
import Database.Engine       ( Entity(..), fromEntity )

-------------------------------------------------------------------------- CRUD

-- | Deletes the scope with the given name.
delete :: String -> App ()
delete name = runDB $ DB.deleteScope name

-- | Inserts a new scope into the database using the given name and expression
-- | or updates an existing scope if one with the same name already exists.
insertOrUpdate :: String -> String -> App Validation
insertOrUpdate name expression = runDB $ do
    let scope   = Scope name expression
        results = validate scope

    when (isValid results) $ do
        existingScope <- DB.selectScope name

        case existingScope of
            Nothing            -> void $ DB.insertScope scope
            Just (Entity id _) -> void $ DB.updateScope (Entity id scope)

    return results

-- | Returns the scope with the given name.
querySingle :: String -> App (Maybe Scope)
querySingle "all" = return (Just (Scope "all" ""))
querySingle name  = fromEntity <$$> runDB (DB.selectScope name)

----------------------------------------------------------------------- Utility

-- | Returns valid if the given scope is valid; otherwise invalid.
validate :: Scope -> Validation
validate (Scope name _) =
    let nameError    = Error "name" name "Invalid name"
        invalidNames = [ defaultName, Path.getDataPrefix, Path.getStaticPrefix]

    in verify (name `notElem` invalidNames) nameError
