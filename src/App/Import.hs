{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Import ( fromDirectory ) where

import qualified App.Core.Post    as Post
import qualified System.Directory as Dir
import qualified System.FilePath  as FilePath
import qualified Text.Parsec      as Parsec

import App.Core.Types       ( App )
import App.Validation       ( Error(..), Validation(..) )
import Control.Exception    ( ErrorCall(..), throwIO )
import Control.Monad.Reader ( MonadIO, filterM, forM_, liftIO, unless, void, when )
import Data.Maybe           ( fromJust )
import Data.Textual         ( splitOn )
import System.FilePath      ( (</>) )
import Text.Parsec          ( ParseError, (<|>), anyChar, between, char, choice
                            , eof, lookAhead, many, manyTill, noneOf, option
                            , spaces, try )

-- | Import each valid file in the given directory into the database. If an
-- | optional output directory is provided, each imported file will be moved
-- | to that directory.
fromDirectory :: FilePath -> Maybe FilePath -> App ()
fromDirectory inPath outPath = do
    liftIO $ do
        isInPathValid  <- Dir.doesDirectoryExist inPath
        isOutPathValid <- maybe (return True) Dir.doesDirectoryExist outPath

        unless isInPathValid  $ throwIO (ErrorCall $ "Invalid path: " ++ inPath)
        unless isOutPathValid $ throwIO (ErrorCall $ "Invalid path: " ++ fromJust outPath)

    liftIO $ do
        putStrLn $ replicate 80 '-'
        putStrLn $ "Importing files from " ++ inPath
        putStrLn $ case outPath of
            Nothing   -> "Successfully imported files will not be moved"
            Just path -> "Successfully imported files will be moved to " ++ path

    files <- getDirectoryFiles inPath
    forM_ files $ \fileName ->
        let parseResults = parseFileName (FilePath.dropExtension fileName)
            filePath     = inPath </> fileName

        in case parseResults of
            Left _ -> do
                logError filePath "Invalid file name pattern"

            Right (title, tags) -> do
                (_, result) <- Post.insert filePath title tags
                case (result, outPath) of
                    (Invalid _, _)     -> logError filePath (show result)
                    (Valid, Just path) -> liftIO $ Dir.renameFile filePath (path </> fileName)
                    _                  -> return ()

    liftIO $ do
        putStrLn $ replicate 80 '-'

----------------------------------------------------------------------- Utility

getDirectoryFiles :: (MonadIO m) => FilePath -> m [String]
getDirectoryFiles path = liftIO $ do
    contents <- Dir.getDirectoryContents path
    filterM (Dir.doesFileExist . (path </>)) contents

-- | Logs an import error message to the console.
logError :: (MonadIO m) => FilePath -> String -> m ()
logError path message = liftIO $ do
    let fileName = FilePath.takeFileName path
    putStrLn $ "Failed to import \"" ++ fileName ++ "\": " ++ message

-- | Parse the given string and return the extracted title and list of tags.
parseFileName :: String -> Either ParseError (String, [String])
parseFileName fileName =
    let anyBetween x y = between (char x) (char y) (many $ noneOf [y])
        anyUntil x     = manyTill anyChar (void (lookAhead $ char x) <|> eof)
        pattern        = do
            tags1 <- option "" $ anyBetween '(' ')' <* spaces
            tags2 <- option "" $ choice
                [ try $ do
                    char '['
                    a <- anyUntil '('
                    b <- anyBetween '(' ')'
                    char ']'
                    return (a ++ "," ++ b)
                , try $ do
                    anyBetween '[' ']'
                ]
            spaces
            tags3 <- option "" $ anyBetween '(' ')'
            title <- anyUntil '('
            tags4 <- option "" $ anyBetween '(' ')'
            spaces
            eof

            return (title, concatMap (splitOn ",") [tags1, tags2, tags3, tags4])

    in Parsec.parse pattern "" fileName
