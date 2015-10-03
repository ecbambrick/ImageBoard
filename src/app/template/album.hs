{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module App.Template.Album where

import App.Common       ( Album(..), Page(..) )
import App.Paths        ( albumThumbnailURL, pageURL, pageThumbnailURL )
import Data.Data        ( Data, Typeable )
import Database.Engine  ( Entity(..), ID )

-- | The context data required for an album.
data AlbumContext = AlbumContext
    { identifier :: ID
    , thumb      :: String
    , pages      :: [PageContext]
    } deriving (Data, Typeable)

-- | Returns the album context data for the given album.
toAlbumContext :: Entity Album -> AlbumContext
toAlbumContext album @ (Entity id Album {..}) = AlbumContext
    { identifier = id
    , thumb      = albumThumbnailURL album
    , pages      = map (toPageContext album) albumPages }

-- | The context data required for a page.
data PageContext = PageContext
    { path      :: String
    , thumbnail :: String
    , number    :: Int
    , previous  :: Int
    , next      :: Int
    } deriving (Data, Typeable)

-- | Returns the page context for the given page.
toPageContext :: Entity Album -> Page -> PageContext
toPageContext (Entity id Album {..}) page @ Page {..} = PageContext
    { path      = pageURL id page
    , thumbnail = pageThumbnailURL id page
    , number    = pageNumber
    , previous  = pageNumber - 1 `mod` length albumPages
    , next      = pageNumber + 1 `mod` length albumPages }

-- | The context data required for a list of albums.
data AlbumsContext = AlbumsContext
    { query        :: String
    , isQuery      :: Bool
    , page         :: Int
    , previousPage :: Int
    , nextPage     :: Int
    , albums       :: [AlbumContext]
    } deriving (Data, Typeable)

-- | Returns the album context for the given query, page, and list of albums.
toAlbumsContext :: String -> Int -> [Entity Album] -> AlbumsContext
toAlbumsContext query page albums = AlbumsContext
    { query        = query
    , isQuery      = not (null query)
    , page         = page
    , previousPage = max (page - 1) 1
    , nextPage     = page + 1
    , albums       = map toAlbumContext albums }
