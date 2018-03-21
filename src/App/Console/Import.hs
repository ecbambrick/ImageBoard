{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module App.Console.Import ( directory, previewDirectory ) where

import qualified App.Core.Post      as Post
import qualified App.Core.Tag       as Tag
import qualified App.Validation     as Validation
import qualified System.Directory   as Dir
import qualified System.FilePath    as FilePath
import qualified System.IO.Metadata as Metadata
import qualified Text.Format        as Format
import qualified Text.Parsec        as Parsec

import App.Core.Types       ( App )
import App.Core.Post        ( PostType(..) )
import Control.Exception    ( ErrorCall(..), throwIO )
import Control.Monad.Reader ( MonadIO, filterM, forM_, liftIO, unless, void )
import Data.Either          ( rights, isRight )
import Data.List            ( (\\), nub, partition, sortBy, union )
import Data.Maybe           ( fromJust )
import Data.Ord.Extended    ( comparingAlphaNum )
import Data.Textual         ( splitOn, toLower, trim )
import Data.Validation      ( Result(..) )
import System.FilePath      ( (</>) )
import Text.Parsec          ( ParseError, (<|>), anyChar, between, char, choice
                            , eof, lookAhead, many, manyTill, noneOf, option
                            , spaces, try )

--------------------------------------------------------------------- Importing

-- | Preview the list of tags that would be created by importing the given
-- | directory.
previewDirectory :: FilePath -> [String] -> App ()
previewDirectory path extraTags = do
    validatePath (Just path)

    existingTags               <- Tag.queryNames Nothing
    (validFiles, invalidFiles) <- partition (isRight . parseFileName) <$> getFiles path
    totalFileSize              <- sum <$> mapM Metadata.getSize validFiles

    let extraTagsResult = Tag.validateNames extraTags
        validExtraTags = case extraTagsResult of
            Success tags -> tags
            Failure _    -> []
        extraTagsErrors = case extraTagsResult of
            Success _ -> []
            Failure e -> e
        pendingTags = nub $ filter (not . null)
                          $ map (toLower . trim)
                          $ concat
                          $ map snd
                          $ rights
                          $ map parseFileName
                          $ validFiles
        newTags = (pendingTags `union` validExtraTags) \\ existingTags

    unless (totalFileSize == 0) $ do
        logInfo $ "File size:\n    " ++ Format.fileSize totalFileSize

    unless (null newTags) $ do
        logInfo $ "New tags:"
        forM_ newTags $ \tag -> do
            logInfo $ "    " ++ tag

    unless (null extraTagsErrors) $ do
        logInfo $ "Errors:"
        forM_ extraTagsErrors $ \err -> do
            logInfo $ "    " ++ Validation.showError err

-- | Import each valid file in the given directory into the database. If an
-- | optional output directory is provided, each imported file will be moved
-- | to that directory. The given list of tags will be added to each imported
-- | file.
directory :: FilePath -> Maybe FilePath -> [String] -> App ()
directory inPath outPath extraTags = do
    validatePath (Just inPath)
    validatePath outPath

    logInfo $ replicate 80 '-'
    logInfo $ "Importing files from " ++ inPath
    logInfo $ case outPath of
        Nothing   -> "Successfully imported files will not be moved"
        Just path -> "Successfully imported files will be moved to " ++ path

    files <- getFiles inPath
    forM_ files $ \filePath ->
        let parseResults = parseFileName filePath
            fileName     = FilePath.takeFileName filePath

        in case parseResults of
            Left _ -> do
                logError filePath "Invalid file name pattern"

            Right (title, tags) -> do
                result <- Post.insert filePath title [] (tags ++ extraTags)
                case (result, outPath) of
                    (InvalidPost e, _) -> logError filePath (Validation.showErrors e)
                    (_,     Just path) -> liftIO $ Dir.renameFile filePath (path </> fileName)
                    _                  -> return ()

    logInfo $ replicate 80 '-'

----------------------------------------------------------------------- Utility

-- | Fails the import if the given path is not valid
validatePath :: (MonadIO m) => Maybe String -> m ()
validatePath path = liftIO $ do
    exists <- maybe (return True) Dir.doesDirectoryExist path

    unless exists $ do
        throwIO (ErrorCall $ "Invalid path: " ++ fromJust path)

-- | Gets the list of valid files (based on file type) from the given directory
-- | in alphanumeric order.
getFiles :: FilePath -> App [String]
getFiles path = do
    allFiles      <- liftIO $ map (path </>) <$> Dir.listDirectory path
    existingFiles <- liftIO $ filterM Dir.doesFileExist allFiles
    validFiles    <- filterM Post.canInsert existingFiles

    return $ sortBy (comparingAlphaNum id) validFiles

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

    in Parsec.parse pattern "" (FilePath.takeBaseName fileName)
