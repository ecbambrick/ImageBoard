module App.Config where

import Control.Applicative ( (<$>), (<*>) )
import Data.Text ( pack )
import Data.Configurator ( Worth(..), load, lookupDefault, require )

-- | Application settings.
data Config = Config 
    { configPort                :: Int
    , configDatabaseConnection  :: String
    , configStoragePath         :: FilePath
    , configPageSize            :: Int
    , configThumbnailSize       :: Int }

-- | Returns the application's settings loaded from the config file.
loadConfig :: IO Config
loadConfig = do
    config <- load [Required "app.cfg"]
    Config <$> lookupDefault 8000 config (pack "port")
           <*> require            config (pack "database")
           <*> require            config (pack "storage_path")
           <*> require            config (pack "page_size")
           <*> require            config (pack "thumbnail_size")
