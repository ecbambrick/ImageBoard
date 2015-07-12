module App.Expression ( Token(..), Expression, parse ) where

import qualified Text.Parsec as Parsec

import Control.Applicative  ( (<$>), (<*>), (<*), (*>), (<|>) )
import Text.Parsec          ( ParseError, many, many1, char, noneOf, spaces )

-------------------------------------------------------------------------- Data

-- | A search expression containing tokens to search by.
type Expression = [Token]

-- | An expression token; included tokens must exist to satisfy the expression
-- | while excluded tokens must not exist to satisfy the expression.
data Token = Included String | Excluded String deriving (Show)

----------------------------------------------------------------------- Parsing

-- | Parses the given string and returns a parse error or an expression.
parse :: String -> Either ParseError Expression
parse = Parsec.parse rules ""
    where 
        rules    = wrapBy (excluded <|> included) spaces
        excluded = char '-' >> token >>= return . Excluded
        included = token >>= return . Included
        token    = (:) <$> noneOf "_ -" <*> many (noneOf " ")

----------------------------------------------------------------------- Utility

-- | Parses one or more occurrences of the given rule, separated by the given
-- | separator, with any leading/trailing instances of the separator.
wrapBy rule separator = many1 (separator *> rule <* separator)
