
module LiName.Filter (
  filterCommands
) where

import LiName.Types

import Control.Applicative ((<$>))
import System.Process (readProcess)



filterCommand :: LiNameFilter -> String -> IO String
filterCommand (LiNameFilter cmd) = readProcess cmd []


filterCommands :: [LiNameFilter] -> [String] -> IO [String]
filterCommands cmds ls = lines <$> filterCommands' cmds (unlines ls)


filterCommands' :: [LiNameFilter] -> String -> IO String
filterCommands' []     src = return src
filterCommands' (x:xs) src = filterCommand x src >>= filterCommands' xs
