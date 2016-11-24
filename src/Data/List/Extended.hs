module Data.List.Extended where

import Data.List ( groupBy )

-- | Groups the given list using the given which projects an element out of
-- | every list element in order to first sort the input list and then to form
-- | groups by equality on these projected elements.
groupWith :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupWith f xs = groupBy (\x y -> f x == f y) xs
