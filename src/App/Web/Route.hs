{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module App.Web.Route where

import qualified App.Path  as Path
import qualified Web.Spock as Spock

import Database.Engine ( ID )
import Web.Spock       ( (<//>), Path )

------------------------------------------------------------------------ Routes

-- | The route to an album with the given ID.
album :: Spock.Path '[String, ID]
album = Spock.var <//> "album" <//> Spock.var

-- | The route to the list of albums.
albums :: Spock.Path '[String]
albums = Spock.var <//> "albums"

-- | The route to the image with the given ID.
image :: Spock.Path '[String, ID]
image = Spock.var <//> "image" <//> Spock.var

-- | The route to the list of images.
images :: Spock.Path '[String]
images = Spock.var <//> "images"

-- | The route to the given page number from the album with the given ID.
page :: Spock.Path '[String, ID, Int]
page = Spock.var <//> "album" <//> Spock.var <//> Spock.var

-- | The route to upload a new post.
upload :: Spock.Path '[String]
upload = Spock.var <//> "upload"

root :: Spock.Path '[]
root = Spock.root
