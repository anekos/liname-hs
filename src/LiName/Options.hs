
module LiName.Options (
  parseOptions
) where

import LiName.Types
import LiName.Sort

import System.Console.GetOpt



header :: String
header = "Usage: liname [OPTION...] <PATH>..."


options :: [OptDescr (LiNameConfig -> LiNameConfig)]
options =
   [ Option "s" ["sort"]
       (ReqArg (\value opts -> opts { _sortType = readSortType value }) "SORT_TYPE")
       (unlines [ "Sort type: m = modification time"
                , "           n = file name"
                , "           p = file path"
                , "           N = file name (case insesitve)"
                , "           P = file path (case insesitve)"
                , "           iX = inverted X" ])
   , Option "i" ["ignore"]
       (ReqArg (\value opts -> opts { _ignore = Just value }) "REGEXP")
       "Ignore pattern"
   , Option "c" ["compact"]
       (ReqArg (\value opts -> opts { _compact = Just $ read value }) "LEVEL")
       "Compact directory part" ]


parseOptions :: LiNameConfig -> [String] -> IO (Either String (LiNameConfig, [String]))
parseOptions conf argv = parseOptions' conf $ getOpt Permute options argv


parseOptions' :: LiNameConfig -> ([LiNameConfig -> LiNameConfig], [String], [String]) -> IO (Either String (LiNameConfig, [String]))
parseOptions' _ (_, [], es) = return $ Left $ usage es
parseOptions' c (o, n, [])  = return $ Right (foldl (flip id) c o, n)
parseOptions' _ (_, _, es)  = return $ Left $ usage es


usage :: [String] -> String
usage errs = concat errs ++ usageInfo header options
