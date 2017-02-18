{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Expression
    ( Token(..), Match(..), Expression, combine, parse, parseMany ) where

import qualified Text.Parsec as Parsec

import App.Core.Types       ( ID )
import Control.Applicative  ( (<|>), empty )
import Data.Either          ( rights )
import Data.List            ( delete, nub )
import Data.Textual         ( intercalate, splitOn, trim )
import Text.Parsec          ( ParseError, anyToken, char, digit, many1, noneOf
                            , oneOf, spaces, string, try )

------------------------------------------------------------------------- Types

-- | A search expression containing tokens to search by.
type Expression = [Token]

-- | An expression token. Included tokens must exist to satisfy the expression
-- | while excluded tokens must not exist to satisfy the expression.
data Token = Included Match | Excluded Match deriving (Show, Eq)

-- | A value to match against in an expression.
data Match = MatchID ID
           | MatchToday
           | MatchThisWeek
           | MatchThisMonth
           | MatchTags String
           deriving (Show, Eq)

----------------------------------------------------------------------- Parsing

-- | Parses the given comma separated string and returns an expression.
-- |
-- | i.e. parse "a, -b  , ,,date:today" == [ Included (MatchTags "a")
-- |                                       , Excluded (MatchTags "b")
-- |                                       , Included MatchToday ]
parse :: String -> Expression
parse = neutralize . nub . rights . map tokenize . map trim . splitOn ","

-- | Parses the given list of comma-separated strings and returns a single
-- | expression.
parseMany :: [String] -> Expression
parseMany = parse . intercalate ","

-- | Converts the given expression to a comma-separated string.
unparse :: Expression -> String
unparse = intercalate ", " . map unparseToken
    where
        unparseToken (Included     x) = unparseMatch x
        unparseToken (Excluded     x) = "-" ++ unparseMatch x
        unparseMatch (MatchID      x) = "id:" ++ show x
        unparseMatch (MatchToday    ) = "date:today"
        unparseMatch (MatchThisWeek ) = "date:this week"
        unparseMatch (MatchThisMonth) = "date:this month"
        unparseMatch (MatchTags    x) = x

-- | Combines the given list of comma-separated strings into a single
-- | comma-separated string.
combine :: [String] -> String
combine = unparse . parseMany

----------------------------------------------------------------------- Utility

-- | Converts the given string to a token. If the first non-space character is
-- | a dash, an excluded token will be returned; otherwise, an included token
-- | will be returned.
tokenize :: String -> Either ParseError Token
tokenize = Parsec.parse rules ""
    where
        rules     = spaces >> (try excluded <|> try included)
        excluded  = negation >> token >>= return . Excluded
        included  =             token >>= return . Included
        token     = tryEach [matchID, matchDate, matchTags]

        matchTags = many1 anyToken      >>= return . MatchTags
        matchID   = special "id" digits >>= return . MatchID . read
        matchDate = special "date" $ tryEach
            [ string "today"      >> return MatchToday
            , string "this week"  >> return MatchThisWeek
            , string "this month" >> return MatchThisMonth ]

        special name rule = string name >> spaces >> char ':' >> spaces >> rule
        tryEach           = foldr (\x y -> try x <|> y) empty
        negation          = many1 (oneOf "- ")
        digits            = many1 digit

-- | Neutralizes any conflicting tokens in the expression. If multuple tokens
-- | with the same value exist but at least one is excluded and one is included
-- | then each token is removed from the expression.
-- |
-- | i.e. neutralize [Included a, Excluded a, Included b] == [Included b]
neutralize :: Expression -> Expression
neutralize = recurse []
    where
        recurse !memo [] = memo
        recurse !memo (x:xs)
            | elem (opposite x) xs = recurse memo (removeAll x xs)
            | otherwise            = recurse (x:memo) xs

        opposite (Included x) = (Excluded x)
        opposite (Excluded x) = (Included x)

        removeAll x xs = filter (\y -> x /= y && opposite x /= y) xs
