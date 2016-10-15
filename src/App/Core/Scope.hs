{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Scope ( getDefault, delete, insertOrUpdate, querySingle ) where

import qualified App.Database   as DB
import qualified App.Path       as Path
import qualified App.Validation as Validation

import App.Control           ( runDB )
import App.Core.Types        ( Scope(..), App )
import App.Validation        ( Error(..), Validation )
import Control.Monad         ( void, when )
import Data.Char             ( isAlphaNum )
import Data.Functor.Extended ( (<$$>) )
import Database.Engine       ( Entity(..), fromEntity )

-------------------------------------------------------------------------- CRUD

getDefault :: Scope
getDefault = Scope defaultName ""

-- | Deletes the scope with the given name.
delete :: String -> App ()
delete name = runDB $ DB.deleteScope name

-- | Inserts a new scope into the database using the given name and expression
-- | or updates an existing scope if one with the same name already exists.
insertOrUpdate :: String -> String -> App Validation
insertOrUpdate name expression = runDB $ do
    let scope   = Scope name expression
        results = validate scope

    when (Validation.isValid results) $ do
        existingScope <- DB.selectScope name

        case existingScope of
            Nothing            -> void $ DB.insertScope scope
            Just (Entity id _) -> void $ DB.updateScope (Entity id scope)

    return results

-- | Returns the scope with the given name.
querySingle :: String -> App (Maybe Scope)
querySingle name =
    if name == defaultName
        then return (Just (Scope defaultName ""))
        else fromEntity <$$> runDB (DB.selectScope name)

----------------------------------------------------------------------- Utility

-- | The scope name for displaying all results.
defaultName :: String
defaultName = "all"

-- | Returns valid if the given scope is valid; otherwise invalid.
validate :: Scope -> Validation
validate (Scope name expr) =
    let invalidNames  = [ defaultName
                        , Path.getDataPrefix
                        , Path.getStaticPrefix ]

    in Validation.validate
        [ Validation.verify (length name > 0) (InvalidScopeName name)
        , Validation.verify (name `notElem` invalidNames) (InvalidScopeName name)
        , Validation.verify (all isAlphaNum name) (InvalidScopeName name)
        , Validation.verify (length expr > 0) (InvalidScopeExpression expr) ]
