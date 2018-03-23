module Data.List.Extended where

import qualified Data.Map as Map

import Data.List ( groupBy, sortBy )

-- | Groups the given list of tuples by the first element. The resulting list
-- | is not sorted in any way.
bundle :: (Ord a) => [(a, b)] -> [(a, [b])]
bundle = Map.toList
       . Map.fromListWith (\x y -> y ++ x)
       . map (\(x, y) -> (x, [y]))

-- | Same as map but with arguments reversed.
foreach = flip map

-- | Groups the given list using the given project function to sort the list
-- | and then form groups by equality on these projected elements.
groupWith :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupWith f xs = groupBy (\x y -> f x == f y) xs

-- | Sorts the givem list using the given projection function.
sortWith :: (Ord b) => (a -> b) -> [a] -> [a]
sortWith f = sortBy (\x y -> compare (f x) (f y))
