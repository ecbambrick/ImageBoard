{-# LANGUAGE FlexibleInstances #-}

module Data.Textual where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy

import Data.Text ( unpack, pack )

class Textual a where
    display     :: (Show b) => b -> a
    intercalate :: a -> [a] -> a
    replace     :: a -> a -> a -> a
    splitOn     :: a -> a -> [a]
    strip       :: a -> a
    toLower     :: a -> a
    toUpper     :: a -> a
    trim        :: a -> a

instance Textual String where
    display     = show
    intercalate = List.intercalate
    replace x y = unpack     . Strict.replace (pack x) (pack y) . pack
    splitOn x   = map unpack . Strict.splitOn (pack x)          . pack
    strip       = unpack     . Strict.strip                     . pack
    toLower     = map Char.toLower
    toUpper     = map Char.toUpper
    trim        = unwords . words

instance Textual Strict.Text where
    display     = Strict.pack . show
    intercalate = Strict.intercalate
    replace     = Strict.replace
    splitOn     = Strict.splitOn
    strip       = Strict.strip
    toLower     = Strict.toLower
    toUpper     = Strict.toLower
    trim        = Strict.unwords . Strict.words

instance Textual Lazy.Text where
    display     = Lazy.pack . show
    intercalate = Lazy.intercalate
    replace     = Lazy.replace
    splitOn     = Lazy.splitOn
    strip       = Lazy.strip
    toLower     = Lazy.toLower
    toUpper     = Lazy.toUpper
    trim        = Lazy.unwords . Lazy.words
