{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Console.Everything where

import App.Core.Everything as Everything

import App.Core.Types      ( App )
import Control.Monad       ( when )
import Control.Monad.Trans ( liftIO )
import Data.Textual        ( toLower )

-- | Deletes all data from the database. If autoYes is false, the user is
-- | prompted to continue.
delete :: Bool -> App ()
delete autoYes = do
    accepted <- liftIO $ if autoYes
                    then do
                        return True
                    else do
                        putStrLn "Are you sure you want to delete all data?"
                        response <- toLower <$> getLine
                        return (response == "y" || response == "yes")

    when accepted Everything.delete
