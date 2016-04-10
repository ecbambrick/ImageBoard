module Main where

import qualified App.Import          as Import
import qualified App.Server          as Server
import qualified System.Console.Args as CLI

main = CLI.cli "Image board." $ do

    -- Run the web server.
    CLI.command "run" $ do
        CLI.run $ Server.run

    -- Import all relevant files from the given directory.
    CLI.command "import" $ do
        path <- CLI.argument "path"
        CLI.run $ Import.fromDirectory path
