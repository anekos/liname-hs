
{-# LANGUAGE OverloadedStrings #-}

module LiName.Utils where

import Data.Text (replace, pack, unpack, Text)



notDots :: String -> Bool
notDots "."  = False
notDots ".." = False
notDots _    = True


byText :: (Text -> Text) -> String -> String
byText f = unpack . f . pack


unbreak :: String -> String
unbreak = byText $ replace "\n" " "
