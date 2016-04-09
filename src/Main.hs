module Main where

import qualified App.Server          as Server
import qualified System.Console.Args as CLI

main = CLI.cli "Image board." (CLI.run Server.run)
