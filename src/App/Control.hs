{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes            #-}

module App.Control where

import qualified App.Core.Everything as Everything
import qualified App.Config          as Config
import qualified System.Directory    as Dir

import App.Config           ( Config(..) )
import App.Core.Types       ( App )
import Control.Monad        ( when )
import Control.Monad.Reader ( MonadReader, ReaderT, ask, asks, local, runReaderT )
import Control.Monad.Trans  ( lift )
import Data.DateTime        ( utcTimeZone )
import Data.Textual         ( splitOn )
import Database.Engine      ( Transaction, execute, runDatabase )
import System.FilePath      ( (</>) )
import Web.Spock            ( SpockT, ActionT, runSpock, spockT )

--------------------------------------------------------------------- Instances

instance (MonadReader r m) => MonadReader r (ActionT m) where
    ask       = lift ask
    local f m = runReaderT (lift m) . f =<< ask

instance (MonadReader r m) => MonadReader r (SpockT m) where
    ask       = lift ask
    local f m = runReaderT (lift m) . f =<< ask

------------------------------------------------------------------ Interpreters

-- | Runs the application with the settings from the config file.
runApplication :: App () -> IO ()
runApplication application = do
    config <- Config.loadConfig

    flip runReaderT config $ do
        Everything.initialize
        application

-- | Runs the server with the settings from the config file.
runServer :: SpockT (ReaderT Config IO) () -> IO ()
runServer routes = do
    config <- Config.loadConfig

    runSpock (configPort config) $ spockT (flip runReaderT config) $ do
        Everything.initialize
        routes

-- | Run a database transaction using the connection string in the application
-- | config.
runDB :: Transaction a -> App a
runDB command = do
    db <- asks configDatabaseConnection
    runDatabase db command

-- | Runs the application using a blank database and a temporary storage
-- | directory. Used for testing.
testApplication :: App a -> IO ()
testApplication application = withTestEnvironment $ \config ->
    runReaderT application config

-- | Runs the application server using a blank database and a temporary storage
-- | directory. Used for testing.
testServer :: SpockT (ReaderT Config IO) () -> IO ()
testServer routes = withTestEnvironment $ \config ->
    runSpock (configPort config) $ spockT (flip runReaderT config) routes

----------------------------------------------------------------------- Utility

-- | Runs the given IO function with a test database and test storage path.
withTestEnvironment :: (Config -> IO a) -> IO ()
withTestEnvironment f = do
    testDir   <- fmap (</> "image board test") Dir.getTemporaryDirectory
    dirExists <- Dir.doesDirectoryExist testDir
    schema    <- readFile "schema.sql"

    when dirExists (Dir.removeDirectoryRecursive testDir)
    Dir.createDirectory testDir

    let database = testDir </> "database"
    writeFile database ""
    runDatabase database $
        let commands = filter (/= "\n") $ splitOn ";" schema
        in  mapM_ execute commands

    f (Config 8000 database testDir 100 256 utcTimeZone)

    Dir.removeDirectoryRecursive testDir
