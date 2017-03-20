{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Console.Scope where

import qualified App.Core.Scope as Scope
import qualified App.Validation as Validation

import App.Core.Types      ( App )
import Control.Monad       ( unless )
import Control.Monad.Trans ( liftIO )

-- | Sets the scope with the given name to the given expression. If no scope
-- | with the given name exists, a new scope is created.
set :: String -> String -> App ()
set name expression = do
    result <- Scope.insertOrUpdate name expression

    unless (Validation.isValid result) $ do
        liftIO $ print result

-- | Removes the scope with the given name. If no scope with the given name
-- | exists, nothing happens.
remove :: String -> App ()
remove name = do
    Scope.delete name
