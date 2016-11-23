{-# LANGUAGE BangPatterns #-}

module App.Expression ( Token(..), Expression, parse, parseMany ) where

import qualified Text.Parsec as Parsec

import Control.Applicative  ( (<$>), (<*>), (<|>) )
import Data.Either          ( rights )
import Data.List            ( delete, nub )
import Data.Textual         ( intercalate, splitOn, trim )
import Text.Parsec          ( ParseError, many1, noneOf, oneOf, spaces )

------------------------------------------------------------------------- Types

-- | A search expression containing tokens to search by.
type Expression = [Token]

-- | An expression token. Included tokens must exist to satisfy the expression
-- | while excluded tokens must not exist to satisfy the expression.
data Token = Included String
           | Excluded String
           deriving (Show, Eq)

----------------------------------------------------------------------- Parsing

-- | Parses the given comma separated string and returns an expression.
-- | i.e. "a,   -b  ,  ,,c" -> [Included "a", Excluded "b", Included "c"]
parse :: String -> Expression
parse = neutralize . nub . rights . map tokenize . splitOn ","

-- | Parses the given list of comma-separated strings and returns a single
-- | expression.
parseMany :: [String] -> Expression
parseMany = parse . intercalate ","

----------------------------------------------------------------------- Utility

-- | Converts the given string to a token. If the first non-space character is
-- | a dash, an excluded token will be returned; otherwise, an included token
-- | will be returned.
tokenize :: String -> Either ParseError Token
tokenize = Parsec.parse rules ""
    where
        rules    = spaces >> (excluded <|> included)
        excluded = many1 (oneOf "- ") >> token >>= return . Excluded . trim
        included =                       token >>= return . Included . trim
        token    = many1 (noneOf ",")

-- | Neutralizes any conflicting tokens in the expression. If multuple tokens
-- | with the same value exist in the expression, only the first one is kept.
-- | i.e. [Included "a",Excluded "a",Included "b"] -> [Included "b"]
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
