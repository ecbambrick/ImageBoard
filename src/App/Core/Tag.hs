{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Core.Tag where

import qualified App.Database   as DB
import qualified App.Validation as Validation

import App.Control      ( runDB )
import App.Core.Types   ( DetailedTag(..), Scope(..), Tag(..), App )
import App.Expression   ( Expression )
import App.Validation   ( Error(..), Validation(..) )
import Control.Monad    ( forM )
import Data.Char        ( isAlphaNum, isSpace )
import Data.Either      ( lefts, rights )
import Data.List        ( nub )
import Data.Textual     ( trim, toLower )

-------------------------------------------------------------------------- CRUD

-- | Returns the list of all tag entities.
query :: Expression -> App [DetailedTag]
query expression = runDB $ do
    tags <- DB.selectTagsDetails expression

    forM tags $ \(tagID, name, postID, imageCount, albumCount) -> do
        categories <- DB.selectTagCategories tagID
        image      <- DB.selectImage postID
        album      <- DB.selectAlbum postID

        let sample = case (image, album) of
                (Just image, _) -> Left image
                (_, Just album) -> Right album

        return (DetailedTag tagID name imageCount albumCount sample categories)

-- | Attach the categories specified by the given list of category names to the
-- | tag with the given name. Returns valid if the the tag and categories each
-- | exist; otherwise invalid.
categorize :: String -> [String] -> App Validation
categorize tagName categoryNames = runDB $ do
    tagID <- DB.selectTagIDByName tagName

    categoryIDs <- forM categoryNames $ \categoryName -> do
        categoryID <- DB.selectCategoryIDByName categoryName
        case categoryID of
            Just id -> return (Right id)
            Nothing -> return (Left categoryName)

    let found   = rights categoryIDs
        missing = lefts  categoryIDs

    case (tagID, found, missing) of
        (Just tid, cids, []) -> DB.attachCategories tid cids >> return Valid
        (Nothing,  _,     _) -> return $ Invalid [TagNotFound tagName]
        (_,        _,    xs) -> return $ Invalid [CategoriesNotFound xs]

----------------------------------------------------------------------- Utility

-- | Sanitizes the given list of tag names.
cleanTags :: [String] -> [String]
cleanTags = filter (not . null) . nub . map (toLower . trim)

-- | Returns valid if all of the given tags are valid; otherwise, invalid.
validateMany :: [Tag] -> Validation
validateMany = Validation.validate . map validate

-- | Returns valid if all fields of the given tag are valid; otherwise invalid.
validate :: Tag -> Validation
validate (Tag name) =
    let isValidStartChar []    = False
        isValidStartChar (x:_) = not (elem x "-:")
        isValidChar x          = isAlphaNum x || isSpace x || elem x "'.-'@!☆?"

    in Validation.validateSingle
        [ Validation.verify (isValidStartChar name) $ InvalidTagName name
        , Validation.verify (all isValidChar name)  $ InvalidTagName name ]
