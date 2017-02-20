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
        [ Validation.verify (isValidStartChar name) $ InvalidTag name
        , Validation.verify (all isValidChar name)  $ InvalidTag name ]
