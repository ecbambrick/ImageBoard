{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Console.Import ( directory ) where

import qualified App.Core.Post    as Post
import qualified App.Validation   as Validation
import qualified System.Directory as Dir
import qualified System.FilePath  as FilePath
import qualified Text.Parsec      as Parsec

import App.Core.Types       ( App )
import App.Core.Post        ( PostType(..) )
import Control.Exception    ( ErrorCall(..), throwIO )
import Control.Monad.Reader ( MonadIO, filterM, forM_, liftIO, unless, void )
import Data.List            ( sortBy )
import Data.Maybe           ( fromJust )
import Data.Ord.Extended    ( comparingAlphaNum )
import Data.Textual         ( splitOn )
import System.FilePath      ( (</>) )
import Text.Parsec          ( ParseError, (<|>), anyChar, between, char, choice
                            , eof, lookAhead, many, manyTill, noneOf, option
                            , spaces, try )

--------------------------------------------------------------------- Importing

-- | Import each valid file in the given directory into the database. If an
-- | optional output directory is provided, each imported file will be moved
-- | to that directory. The given list of tags will be added to each imported
-- | file.
directory :: FilePath -> Maybe FilePath -> [String] -> App ()
directory inPath outPath extraTags = do
    liftIO $ do
        isInPathValid  <- Dir.doesDirectoryExist inPath
        isOutPathValid <- maybe (return True) Dir.doesDirectoryExist outPath

        unless isInPathValid  $ throwIO (ErrorCall $ "Invalid path: " ++ inPath)
        unless isOutPathValid $ throwIO (ErrorCall $ "Invalid path: " ++ fromJust outPath)

    logInfo $ replicate 80 '-'
    logInfo $ "Importing files from " ++ inPath
    logInfo $ case outPath of
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
                result <- Post.insert filePath title (tags ++ extraTags)
                case (result, outPath) of
                    (InvalidPost e, _) -> logError filePath (Validation.showErrors e)
                    (_,     Just path) -> liftIO $ Dir.renameFile filePath (path </> fileName)
                    _                  -> return ()

    logInfo $ replicate 80 '-'

----------------------------------------------------------------------- Utility

-- | Gets the list of files from the given directory in alphanumeric order.
getDirectoryFiles :: (MonadIO m) => FilePath -> m [String]
getDirectoryFiles path = do
    contents <- liftIO $ Dir.listDirectory path
    files    <- liftIO $ filterM (Dir.doesFileExist . (path </>)) contents

    return $ sortBy (comparingAlphaNum id) files

-- | Logs an import error message to the console.
logError :: (MonadIO m) => FilePath -> String -> m ()
logError path message = liftIO $ do
    let fileName = FilePath.takeFileName path
    putStrLn $ "Failed to import \"" ++ fileName ++ "\": " ++ message

-- | Logs an information message to the console.
logInfo :: (MonadIO m) => String -> m ()
logInfo = liftIO . putStrLn

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
