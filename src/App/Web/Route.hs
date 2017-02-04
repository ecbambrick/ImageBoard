{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module App.Web.Route where

import qualified App.Path  as Path
import qualified Web.Spock as Spock

import App.Core.Types          ( ID )
import Web.Spock               ( (<//>), Path )
import Web.Routing.Combinators ( PathState(..) )

------------------------------------------------------------------------ Routes

-- | The route to an album with the given ID.
album :: Path '[String, ID] Open
album = Spock.var <//> "album" <//> Spock.var

-- | The route to the list of albums.
albums :: Path '[String] Open
albums = Spock.var <//> "albums"

-- | The route to the image with the given ID.
image :: Path '[String, ID] Open
image = Spock.var <//> "image" <//> Spock.var

-- | The route to the list of images.
images :: Path '[String] Open
images = Spock.var <//> "images"

-- | The route to the given page number from the album with the given ID.
page :: Path '[String, ID, Int] Open
page = Spock.var <//> "album" <//> Spock.var <//> Spock.var

-- | The route to the list of tags.
tags :: Path '[String] Open
tags = Spock.var <//> "tags"

-- | The route to upload a new post.
upload :: Path '[String] Open
upload = Spock.var <//> "upload"

root :: Path '[] Open
root = Spock.root
