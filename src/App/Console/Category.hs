{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Console.Category where

import qualified App.Core.Tag as Tag

import App.Core.Types         ( SimpleTag(..), App )
import App.Validation         ( Validation(..) )
import Control.Monad          ( unless )
import Control.Monad.Trans    ( liftIO )
import Control.Monad.Extended ( recurseOn )
import Data.DateTime          ( DateTime )

-- | Asks the user to categorize each uncategorized tag that was created since
-- | the given date.
categorizeUnassignedTags :: DateTime -> App ()
categorizeUnassignedTags dateTime = do
    tags <- Tag.queryRecentUncategorized dateTime

    recurseOn tags $ \recurse tags -> do
        unless (null tags) $ do
            let (x:xs) = tags

            liftIO $ putStr (tagName x ++ ": ")
            response <- liftIO $ getLine

            case response of
                ""   -> recurse xs
                ".q" -> return ()
                name -> do
                    result <- Tag.categorize (tagName x) (words name)
                    case result of
                        Valid     -> recurse xs
                        Invalid _ -> liftIO (print result) >> recurse (x:xs)
