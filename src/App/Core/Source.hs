module App.Core.Source where

import App.Validation ( Error(..), Result(..), Validation )
import Data.Char      ( isSpace )
import Data.List      ( nub )
import Network.URI    ( parseURI )

-- | Returns a trimmed version of the source URL if valid; otherwise, a failure
-- | is returned.
validateURL :: String -> Validation String
validateURL url = case parseURI url of
    Nothing  -> Failure [InvalidSourceURL url]
    Just uri -> Success (show uri)

-- | If each of the given strings are a valid source URL, a trimmed version of
-- | each is returned; otherwise, a failure is returned.
validateURLs :: [String] -> Validation [String]
validateURLs = sequence . nub . map validateURL . filter (not . all isSpace)
