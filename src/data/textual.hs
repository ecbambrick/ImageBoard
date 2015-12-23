{-# LANGUAGE FlexibleInstances #-}

module Data.Textual where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy

import Data.Text ( unpack, pack )

class Textual a where
    intercalate :: a -> [a] -> a
    replace     :: a -> a -> a -> a
    splitOn     :: a -> a -> [a]
    strip       :: a -> a
    toLower     :: a -> a
    trim        :: a -> a

instance Textual String where
    replace x y = unpack     . Strict.replace (pack x) (pack y) . pack
    splitOn x   = map unpack . Strict.splitOn (pack x)          . pack
    strip       = unpack     . Strict.strip                     . pack
    toLower     = map Char.toLower
    trim        = unwords . words
    intercalate = List.intercalate

instance Textual Strict.Text where
    replace     = Strict.replace
    splitOn     = Strict.splitOn
    strip       = Strict.strip
    toLower     = Strict.toLower
    trim        = Strict.unwords . Strict.words
    intercalate = Strict.intercalate

instance Textual Lazy.Text where
    replace     = Lazy.replace
    splitOn     = Lazy.splitOn
    strip       = Lazy.strip
    toLower     = Lazy.toLower
    trim        = Lazy.unwords . Lazy.words
    intercalate = Lazy.intercalate
