module Text.Format where

import qualified Numeric

-- | Formats the given integral value as a file size.
fileSize :: (Integral a) => a -> String
fileSize x
    | x <= 10^3 = "1kb"
    | x >= 10^6 = Numeric.showFFloat (Just 1) (fromIntegral x / 1000000) "mb"
    | otherwise = Numeric.showFFloat (Just 0) (fromIntegral x / 1000)    "kb"
