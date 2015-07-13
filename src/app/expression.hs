module App.Expression ( Token(..), Expression, parse ) where

import qualified Text.Parsec as Parsec
import qualified Data.Set as Set

import Control.Applicative  ( (<$>), (<*>), (<*), (*>), (<|>) )
import Data.List            ( delete )
import Text.Parsec          ( ParsecT, ParseError, Stream, many, many1, char
                            , noneOf, spaces )

-------------------------------------------------------------------------- Data

-- | A search expression containing tokens to search by.
type Expression = [Token]

-- | An expression token; included tokens must exist to satisfy the expression
-- | while excluded tokens must not exist to satisfy the expression.
data Token = Included String | Excluded String deriving (Show, Eq, Ord)

----------------------------------------------------------------------- Parsing

-- | Parses the given string and returns a parse error or an expression.
parse :: String -> Either ParseError Expression
parse x = neutralize <$> removeDuplicates <$> Parsec.parse rules "" x
    where
        rules    = wrapBy (excluded <|> included) spaces
        excluded = char '-' >> token >>= return . Excluded
        included = token >>= return . Included
        token    = (:) <$> noneOf "_ -" <*> many (noneOf " ")

----------------------------------------------------------------------- Utility

-- | Parses one or more occurrences of the given rule, separated by the given
-- | separator, with any leading/trailing instances of the separator.
wrapBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a] 
wrapBy rule separator = many1 (separator *> rule <* separator)

-- | Removes duplicated tokens from the expression.
removeDuplicates :: Expression -> Expression
removeDuplicates = Set.toList . Set.fromList

-- | Neutralizes any conflicting tokens in the expression. If an included token
-- | and excluded token both contain the same string, both are removed.
-- | i.e. [Included "a",Excluded "a",Included "b"] -> [Included "b"]
neutralize :: Expression -> Expression
neutralize = recurse []
    where
        recurse memo [] = memo
        recurse memo (x:xs)
            | elem y xs = recurse (memo) (delete y xs)
            | otherwise = recurse (x:memo) (xs)
            where y = oppose x

-- | Returns the opposite of the given token. If an included token is passed in,
-- | an excluded token is returned and vice-versa.
oppose :: Token -> Token
oppose (Included x) = (Excluded x)
oppose (Excluded x) = (Included x)
