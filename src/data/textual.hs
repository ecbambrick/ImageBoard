{-# LANGUAGE FlexibleInstances #-}
module Data.Textual where

import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy
import qualified Data.Char as Char

class Textual a where
    replace :: a -> a -> a -> a
    splitOn :: a -> a -> [a]
    strip   :: a -> a
    toLower :: a -> a

instance Textual String where
    replace x y = Strict.unpack . Strict.replace (Strict.pack x) (Strict.pack y) . Strict.pack
    splitOn x = map Strict.unpack . Strict.splitOn (Strict.pack x) . Strict.pack
    strip = Strict.unpack . Strict.strip . Strict.pack
    toLower = map Char.toLower

instance Textual Strict.Text where
    replace = Strict.replace
    splitOn = Strict.splitOn
    strip = Strict.strip
    toLower = Strict.toLower

instance Textual Lazy.Text where
    replace = Lazy.replace
    splitOn = Lazy.splitOn
    strip = Lazy.strip
    toLower = Lazy.toLower
