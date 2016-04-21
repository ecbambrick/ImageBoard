{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Main where

import qualified App.Control         as Application
import qualified App.Import          as Import
import qualified App.Path            as Path
import qualified App.Server          as Server
import qualified System.Console.Args as CLI

main = CLI.cli "Image board." $ do

    -- Run the web server.
    CLI.command "run" $ do
        CLI.run $ Application.runServer Server.routes

    -- Import all relevant files from the given directory.
    CLI.command "import" $ do
        path <- CLI.argument "path"

        CLI.run $ Import.fromDirectory path
