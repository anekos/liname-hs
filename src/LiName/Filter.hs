
module LiName.Filter (
  filterCommands
) where

import LiName.Types

import System.Process (readProcess)



filterCommand :: LiNameFilterCommand -> String -> IO String
filterCommand (LiNameFilterCommand cmd) = readProcess cmd []


filterCommands :: [LiNameFilterCommand] -> String -> IO String
filterCommands []     src = return src
filterCommands (x:xs) src = filterCommand x src >>= filterCommands xs
