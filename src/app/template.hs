module App.Template where

import qualified Data.Text.IO as Text

import App.Paths                ( templatePath )
import Control.Monad.Trans      ( MonadIO, liftIO )
import Data.Data                ( Data )
import Data.Text                ( Text )
import Data.Text.Lazy           ( toStrict )
import Text.Hastache            ( hastacheStr, defaultConfig )
import Text.Hastache.Context    ( mkGenericContext )

-- | Renders the mustahce template with the given name with the given data.
render :: (MonadIO m, Data a) => FilePath -> a -> m Text
render name context = do
    let genericContext = mkGenericContext context
    template <- liftIO $ Text.readFile (templatePath name)
    result   <- liftIO $ hastacheStr defaultConfig template genericContext
    return (toStrict result)
