module Data.Ord.Extended where

import Data.Char ( isNumber )

-- | Useful combinator for use in conjunction with the xxxBy family of
-- | functions from Data.List, but specifically for strings that contain
-- | numeric substrings.
comparingAlphaNum :: (a -> String) -> a -> a -> Ordering
comparingAlphaNum f x y = compareAlphaNum (f x) (f y)

-- | Compares two strings containing zero or more numbers. Differs from a
-- | normal string comparison in that it will compare numeric substrings as if
-- | they were actual numbers rather than strings.
-- |
-- | i.e. "a20" > "a100" = true
compareAlphaNum :: String -> String -> Ordering
compareAlphaNum  [x]  [y]  = compare x y
compareAlphaNum  [ ] (_:_) = LT
compareAlphaNum (_:_) [ ]  = GT
compareAlphaNum (x:xs) (y:ys)
    | isNumber x && isNumber y =
        let (numberX, restX) = span isNumber (x:xs)
            (numberY, restY) = span isNumber (y:ys)
            a                = read numberX :: Int
            b                = read numberY :: Int

        in if a /= b || null restX && null restY
            then compare a b
            else compareAlphaNum restX restY

    | x == y    = compareAlphaNum xs ys
    | otherwise = compare x y
