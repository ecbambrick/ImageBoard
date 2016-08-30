{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified App.Control         as Application
import qualified App.Core.Everything as Everything
import qualified App.Core.Scope      as Scope
import qualified App.Import          as Import
import qualified App.Web.Server      as Server
import qualified System.Console.Args as CLI

import App.Validation       ( isValid )
import Control.Monad.Reader ( liftIO, when )
import Data.Functor         ( (<$>) )
import Data.Textual         ( toLower )

main = CLI.cli "Image board." $ do
    let runApplication = CLI.run . Application.runApplication
        runServer      = CLI.run . Application.runServer

    -- Run the web server.
    CLI.command "run" $ do
        runServer Server.routes

    -- Import all relevant files from the given directory.
    CLI.command "import" $ do
        path      <- CLI.argument "path"
        moveFiles <- CLI.option ('m', "move") "Moves files to the given directory after being imported."

        runApplication $ do
            Import.fromDirectory path moveFiles

    -- Delete all data from the database.
    CLI.command "delete-all-data" $ do
        autoYes <- CLI.option ('y', "auto-yes") "Automatically answer yes to prompts."

        runApplication $ do
            continue <- if autoYes
                then do
                    return True
                else do
                    liftIO $ putStrLn "Are you sure you want to delete all data?"
                    response <- liftIO $ toLower <$> getLine

                    return (response == "y" || response == "yes")

            when continue $ do
                Everything.delete

    -- | Manage scopes.
    CLI.command "scope" $ do

        CLI.command "set" $ do
            name       <- CLI.argument "name"
            expression <- CLI.argument "expression"

            runApplication $ do
                result <- Scope.insertOrUpdate name expression

                when (not (isValid result)) $ do
                    liftIO $ print result

        CLI.command "remove" $ do
            name <- CLI.argument "name"

            runApplication $ do
                Scope.delete name
