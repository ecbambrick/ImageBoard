{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.View where

import qualified App.Core.Album as Album

import App.Common       ( Album(..), Page(..) )
import App.Paths        ( getPageThumbnailURL )
import Control.Monad    ( forM_, unless )
import Data.Text        ( Text, pack, empty )
import Data.Text.Lazy   ( toStrict )    
import Database.Engine  ( Entity(..) )    
import Lucid.Base       ( ToHtml(..), Html, renderText )
import Lucid.Html5

------------------------------------------------------------------------- Views

-- | Renders a page for the given album as text containing HTML.
albumPage :: Entity Album -> Text
albumPage (Entity id album @ Album {..}) =
    render $ document ("Album " ++ show id) [] $ do
    
        unless (null albumTagNames) $
            ul_ [ id_ "tags", class_ "listBox" ] $ 
                forM_ albumTagNames (li_ . toHtml)
    
        forM_ albumPages $ \(Page _ number _) -> do
            let page  = Album.getPage album number
                link  = pack $ "/album/" ++ show id ++ "/" ++ show number
                thumb = case page of
                    Nothing   -> ""
                    Just page -> pack $ getPageThumbnailURL id page
            
            a_ [href_ link] $ img_ [src_ thumb]

----------------------------------------------------------------------- Utility

-- | Creates an HTML document with the given title, list of javascript import
-- | paths and HTML child as the body.
document :: String -> [Text] -> Html a -> Html a
document title imports f = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        meta_  [content_ "text/html;charset=utf-8",  httpEquiv_ "Content-Type"]
        link_  [rel_ "stylesheet", type_ "text/css", href_ "/static/style.css"]
        forM_ imports $ \x -> 
            script_ [type_ "application/javascript;version=1.7", src_ x] empty
    
    body_ f

-- | Renders the given HTML as text.
render :: Html a -> Text
render = toStrict . renderText
