{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Console.Tag ( list ) where

import qualified App.Core.Tag as Tag

import App.Core.Types      ( App )
import Control.Monad       ( mapM_ )
import Control.Monad.Trans ( liftIO )

-- | Show the list of tags that match the given query. If no query is provided,
-- | the list of all tags is shown.
list :: Maybe String -> App ()
list query = do
    tagNames <- Tag.queryNames query
    liftIO $ mapM_ print tagNames
