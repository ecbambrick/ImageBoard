module App.FileType 
    ( FileType(..), File(..), ImageFile, getFileType ) where

import System.FilePath ( takeExtension )
import Data.Textual    ( toLower )

-- | The type of a file in terms of its usage within the application.
data FileType = ImageType   ImageFile 
              | InvalidType String

-- | An image file descriptor containing the file path and extension.
data ImageFile = ImageFile FilePath String

-- | Class of file data structures that contain a path and extension.
class File a where
    getPath      :: a -> FilePath
    getExtension :: a -> String

instance File ImageFile where
    getPath      (ImageFile path _) = path
    getExtension (ImageFile _  ext) = ext

-- | The list of valid image file types.
validImageTypes = [ "bmp", "gif", "jpg", "jpeg", "png" ]

-- | Gets the file type from the given path.
getFileType :: FilePath -> String -> FileType
getFileType path name
    | elem ext validImageTypes   = ImageType   $ ImageFile   path ext
    | otherwise                  = InvalidType $ ext
    where ext = toLower $ drop 1 $ takeExtension name
