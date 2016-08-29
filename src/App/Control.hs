{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes            #-}

module App.Control where

import qualified App.Core.Everything as Everything
import qualified App.Path            as Path

import App.Config           ( Config(..), loadConfig )
import App.Core.Types       ( App )
import Control.Monad        ( when )
import Control.Monad.Reader ( MonadReader, ReaderT, ask, asks, local, runReaderT )
import Control.Monad.Trans  ( MonadIO, lift )
import Data.DateTime        ( utcTimeZone )
import Data.Textual         ( splitOn )
import Database.Engine      ( Transaction, execute, runDatabase )
import System.Directory     ( createDirectory, doesDirectoryExist
                            , getTemporaryDirectory, removeDirectoryRecursive )
import System.FilePath      ( (</>) )
import Web.Spock            ( SpockT, ActionT, runSpock, spockT )

--------------------------------------------------------------------- Instances

instance (MonadReader r m) => MonadReader r (ActionT m) where
    ask       = lift ask
    local f m = runReaderT (lift m) . f =<< ask

instance (MonadReader r m) => MonadReader r (SpockT m) where
    ask       = lift ask
    local f m = runReaderT (lift m) . f =<< ask

------------------------------------------------------------------ interpreters

-- | Runs the application with the settings from the config file.
runApplication :: App () -> IO ()
runApplication application = do
    config <- loadConfig

    flip runReaderT config $ do
        Everything.initialize
        application

-- | Runs the server with the settings from the config file.
runServer :: SpockT (ReaderT Config IO) () -> IO ()
runServer routes = do
    config <- loadConfig

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
testApplication application = do
    testDir   <- fmap (</> "testing") getTemporaryDirectory
    dirExists <- doesDirectoryExist testDir
    schema    <- readFile "schema.sql"

    let database = testDir </> "database"

    when dirExists (removeDirectoryRecursive testDir)
    createDirectory testDir
    writeFile database ""

    runDatabase database $
        let commands = filter (/= "\n") $ splitOn ";" schema
        in  mapM_ execute commands

    runReaderT application (Config 8000 database testDir 100 256 utcTimeZone)

    removeDirectoryRecursive testDir
