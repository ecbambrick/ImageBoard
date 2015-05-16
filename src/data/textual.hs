{-# LANGUAGE FlexibleInstances #-}
module Data.Textual where

import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy

class Textual a where
    splitOn :: a -> a -> [a]
    strip :: a -> a

instance Textual String where
    splitOn x = map Strict.unpack . Strict.splitOn (Strict.pack x) . Strict.pack
    strip = Strict.unpack . Strict.strip . Strict.pack

instance Textual Strict.Text where
    splitOn = Strict.splitOn
    strip = Strict.strip

instance Textual Lazy.Text where
    splitOn = Lazy.splitOn
    strip = Lazy.strip
