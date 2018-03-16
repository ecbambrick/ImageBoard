{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Tag where

import qualified App.Storage.Database as DB
import qualified App.Validation       as Validation

import App.Control      ( runDB )
import App.Core.Types   ( App, DetailedTag, SimpleTag )
import App.Expression   ( Expression )
import App.Validation   ( Error(..), Validation )
import Control.Monad    ( forM, void )
import Data.Char        ( isAlphaNum, isSpace )
import Data.DateTime    ( DateTime )
import Data.Either      ( lefts, rights )
import Data.List        ( nub )
import Data.Maybe       ( fromJust, isJust )
import Data.Textual     ( trim, toLower )

----------------------------------------------------------------------- Queries

-- | Returns the list of all tag names. If a string is given, the list is
-- | filtered by all tags that begin with the given string.
queryNames :: Maybe String -> App [String]
queryNames = runDB . DB.selectTagNames

-- | Returns a detailed list of all tags that match the given expression.
queryDetailed :: Expression -> App [DetailedTag]
queryDetailed = runDB . DB.selectDetailedTags

-- | Returns all uncategorized tags that have been created since the given date
-- | and time.
queryRecentUncategorized :: DateTime -> App [SimpleTag]
queryRecentUncategorized = runDB . DB.selectRecentUncategorizedTags

---------------------------------------------------------------------- Commands

-- | Attach the categories specified by the given list of category names to the
-- | tag with the given name. Returns valid if the the tag and categories each
-- | exist; otherwise invalid.
categorize :: String -> [String] -> App (Validation ())
categorize tagName categoryNames = runDB $ do
    tagID <- DB.selectTagIDByName tagName

    categoryIDs <- forM categoryNames $ \categoryName -> do
        categoryID <- DB.selectCategoryIDByName categoryName
        return $ case categoryID of
            Just id -> Right id
            Nothing -> Left categoryName

    let found   = rights categoryIDs
        missing = lefts  categoryIDs
        result  = Validation.assert (isJust tagID) [TagNotFound tagName]
               *> Validation.assert (null missing) [CategoriesNotFound missing]

    Validation.whenSuccess result $ \_ -> do
        DB.attachCategories (fromJust tagID) found

    return (void result)

-------------------------------------------------------------------- Validation

-- | If the given string is a valid tag name, a trimmed version is returned;
-- | otherwise, a failure is returned.
validateName :: String -> Validation String
validateName tag = do
    let isValidStartChar []    = False
        isValidStartChar (x:_) = not (elem x "-:")
        isValidChar x          = isAlphaNum x || isSpace x || elem x "'.-'@!☆?"

    Validation.assert (isValidStartChar tag) [InvalidTagName tag]
    Validation.assert (all isValidChar  tag) [InvalidTagName tag]
    Validation.reject (all isSpace      tag) [InvalidTagName tag]

    return $ toLower $ trim tag

-- | If each of the given strings are a valid tag name, a trimmed version of
-- | each is returned; otherwise, a filure is returned.
validateNames :: [String] -> Validation [String]
validateNames = sequence . nub . map validateName . filter (not . all isSpace)
