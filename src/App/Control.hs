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
import Web.Spock            ( SpockActionCtx, SpockM, runSpock, spock, getState )
import Web.Spock.Config     ( PoolOrConn(..), defaultSpockCfg )

--------------------------------------------------------------------- Instances

instance MonadReader r (SpockActionCtx ctx conn sess r) where
    ask       = getState
    local f m = runReaderT (lift m) . f =<< ask

instance MonadReader r (SpockM conn sess r) where
    ask       = getState
    local f m = runReaderT (lift m) . f =<< ask

------------------------------------------------------------------ Interpreters

-- | Runs the application with the settings from the config file.
runApplication :: ReaderT Config IO () -> IO ()
runApplication application = do
    config <- Config.loadConfig

    flip runReaderT config $ do
        Everything.initialize
        application

-- | Runs the server with the settings from the config file.
runServer :: SpockM () () Config () -> IO ()
runServer routes = do
    appConfig   <- Config.loadConfig
    spockConfig <- defaultSpockCfg () PCNoDatabase appConfig

    runSpock (configPort appConfig) $ spock spockConfig $ do
        Everything.initialize
        routes

-- | Runs the application using a blank database and a temporary storage
-- | directory. Used for testing.
-- testApplication :: App () -> IO ()
testApplication :: ReaderT Config IO () -> IO ()
testApplication application = withTestEnvironment $ \config -> do
    runReaderT application config

-- | Runs the application server using a blank database and a temporary storage
-- | directory. Used for testing.
testServer :: SpockM () () Config () -> IO ()
testServer routes = withTestEnvironment $ \appConfig -> do
    spockConfig <- defaultSpockCfg () PCNoDatabase appConfig
    runSpock (configPort appConfig) $ spock spockConfig routes

-- | Run a database transaction using the connection string in the application
-- | config.
runDB :: Transaction a -> App a
runDB command = do
    db <- asks configDatabaseConnection
    runDatabase db command

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

    f (Config 8000 database testDir 100 512 utcTimeZone)

    Dir.removeDirectoryRecursive testDir
