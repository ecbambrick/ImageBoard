{-# LANGUAGE OverloadedStrings #-}

module App.Web.Icon where

import Data.Text ( Text )

-- | A UI icon.
data Icon
    = LeftArrow
    | RightArrow
    | UpArrow
    | DownArrow
    | Grid
    | Pencil
    | Trash
    | Check
    | Cross
    | Search
    | Stop
    | Pause
    | Image
    | Book
    | LevelDown

-- | Renders the given icon as a CSS class.
render :: Icon -> Text
render icon = case icon of
    LeftArrow  -> "fa-arrow-left"
    RightArrow -> "fa-arrow-right"
    UpArrow    -> "fa-arrow-up"
    DownArrow  -> "fa-arrow-down"
    Grid       -> "fa-th-large"
    Pencil     -> "fa-pencil"
    Trash      -> "fa-trash"
    Check      -> "fa-check"
    Cross      -> "fa-times"
    Search     -> "fa-search"
    Stop       -> "fa-stop"
    Pause      -> "fa-pause"
    Image      -> "fa-image"
    Book       -> "fa-book"
    LevelDown  -> "fa-level-down"
