{-# LANGUAGE FlexibleContexts #-}

module App.Import ( fromDirectory ) where

import qualified App.Core.Album as Album
import qualified App.Core.Image as Image
import qualified Text.Parsec    as Parsec

import App.Config             ( loadConfig )
import App.FileType           ( FileType(..), getFileType )
import Control.Applicative    ( (<|>) )
import Control.Monad.Reader   ( void, forM_, runReaderT )
import Data.Functor.Identity  ( Identity )
import Data.Textual           ( splitOn, intercalate )
import System.FilePath        ( (</>), dropExtension )
import System.Directory       ( getDirectoryContents )
import Text.Parsec            ( ParsecT, ParseError, Stream, anyChar, anyToken
                              , between, char, choice, eof, lookAhead, many
                              , manyTill, noneOf, option, spaces, try )

-- | Import each valid file in the given directory into the database.
fromDirectory :: FilePath -> IO ()
fromDirectory path = do
    config <- loadConfig
    files  <- getDirectoryContents path

    flip runReaderT config $ do
        forM_ files $ \fileName ->
            let parseResults = parse $ dropExtension fileName
                fileResults  = getFileType filePath fileName
                filePath     = path </> fileName

            in case parseResults of
                Left err            -> return ()
                Right (title, tags) -> case fileResults of
                    ArchiveType file -> void $ Album.insert file title tags
                    ImageType   file -> void $ Image.insert file title tags
                    InvalidType    _ -> return ()

-- | Parse the given string and return the extracted title and list of tags.
parse :: String -> Either ParseError (String, [String])
parse x =
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

    in Parsec.parse pattern "" x
