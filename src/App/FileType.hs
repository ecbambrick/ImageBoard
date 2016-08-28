module App.FileType
    ( FileType(..), File(..), ImageFile, ArchiveFile, getFileType
    , validImageTypes ) where

import System.FilePath ( takeExtension )
import Data.Textual    ( toLower )

-- | The type of a file in terms of its usage within the application.
data FileType = ArchiveType ArchiveFile
              | ImageType   ImageFile
              | InvalidType String

-- | An image file descriptor containing the file path and extension.
data ImageFile = ImageFile FilePath String

-- | An archive file descriptor containing the file path and extension.
data ArchiveFile = ArchiveFile FilePath String

-- | Class of file data structures that contain a path and extension.
class File a where
    getPath      :: a -> FilePath
    getExtension :: a -> String

instance File ArchiveFile where
    getPath      (ArchiveFile path _) = path
    getExtension (ArchiveFile _  ext) = ext

instance File ImageFile where
    getPath      (ImageFile path _) = path
    getExtension (ImageFile _  ext) = ext

-- | The list of valid image file types.
validImageTypes = [ "bmp", "gif", "jpg", "jpeg", "png", "webm" ]

-- | The list of valid archive file types.
validArchiveTypes = [ "cbz", "zip" ]

-- | Gets the file type from the given path.
getFileType :: FilePath -> String -> FileType
getFileType path name
    | elem ext validImageTypes   = ImageType   $ ImageFile   path ext
    | elem ext validArchiveTypes = ArchiveType $ ArchiveFile path ext
    | otherwise                  = InvalidType $ ext
    where ext = toLower $ drop 1 $ takeExtension name
