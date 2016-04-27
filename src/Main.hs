{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Main where

import qualified App.Control         as Application
import qualified App.Core.Everything as Everything
import qualified App.Import          as Import
import qualified App.Path            as Path
import qualified App.Web.Server      as Server
import qualified System.Console.Args as CLI

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
        path <- CLI.argument "path"

        runApplication $ do
            Import.fromDirectory path

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
