
{-# LANGUAGE OverloadedStrings #-}

module LiName.Utils where

import LiName.Types

import Data.Maybe (isNothing)
import Data.Text (replace, pack, unpack, Text)
import Text.Regex (mkRegex, matchRegex)



notDots :: String -> Bool
notDots "."  = False
notDots ".." = False
notDots _    = True


byText :: (Text -> Text) -> String -> String
byText f = unpack . f . pack


unbreak :: String -> String
unbreak = byText $ replace "\n" " "


addDelim :: LiNamePath -> LiNamePath
addDelim = (++ "/")


makeNotMatcher :: String -> String -> Bool
makeNotMatcher s = let re = mkRegex s
                   in  isNothing . matchRegex re


indent :: String -> String
indent = ("  " ++)
