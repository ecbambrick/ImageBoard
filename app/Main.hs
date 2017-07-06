{-# LANGUAGE FlexibleContexts #-}

import qualified App.Console.Category   as Console.Category
import qualified App.Console.Everything as Console.Everything
import qualified App.Console.Import     as Console.Import
import qualified App.Console.Scope      as Console.Scope
import qualified App.Control            as App
import qualified App.Web.Server         as Server
import qualified Data.DateTime          as DateTime
import qualified System.Console.Args    as CLI

import Data.Textual  ( splitOn )
import Control.Monad ( when )

main = CLI.cli "Image board." $ do
    isTesting <- CLI.option "test" "Run the command in a temporary test environment."

    let runApplication = if isTesting then CLI.run . App.testApplication
                                      else CLI.run . App.runApplication
        runServer      = if isTesting then CLI.run . App.testServer
                                      else CLI.run . App.runServer

    -- Run the web server.
    CLI.command "run" $ do
        runServer Server.routes

    -- Delete all data from the database.
    CLI.command "delete-all-data" $ do
        autoYes <- CLI.option ('y', "auto-yes") "Automatically answer yes to prompts."
        runApplication $ Console.Everything.delete autoYes

    -- Import all relevant files from the given directory.
    CLI.command "import" $ do
        inPath     <- CLI.argument "path"
        extraTags  <- CLI.option ('t', "tags") "Include tags for each imported file."
        moveFiles  <- CLI.option ('o', "out") "Move files to the given directory after importing."
        categorize <- CLI.option ('c', "categorize") "Categorize new tags after importing."
        preview    <- CLI.option ('p', "preview") "Preview import details instead of importing."

        let outPath = if isTesting then Nothing else moveFiles
            tags    = maybe [] (splitOn ",") extraTags

        runApplication $ do
            if preview
                then do
                    Console.Import.previewDirectory inPath
                else do
                    now <- DateTime.now
                    Console.Import.directory inPath outPath tags
                    when categorize $ Console.Category.categorizeUnassignedTags now

    -- | Manage scopes.
    CLI.command "scope" $ do

        CLI.command "set" $ do
            name       <- CLI.argument "name"
            expression <- CLI.argument "expression"
            runApplication $ Console.Scope.set name expression

        CLI.command "remove" $ do
            name <- CLI.argument "name"
            runApplication $ Console.Scope.remove name
