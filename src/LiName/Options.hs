
module LiName.Options (
  parseOptions
) where

import LiName.Sort
import LiName.Types

import Control.Lens
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
       (ReqArg (\value opts -> opts { _ignoreName = Just value }) "REGEXP")
       "Ignore pattern for file name"
   , Option "I" ["ignore-path"]
       (ReqArg (\value opts -> opts { _ignorePath = Just value }) "REGEXP")
       "Ignore pattern for path"
   , Option "c" ["compact"]
       (ReqArg (\value opts -> opts { _compact = Just $ read value }) "LEVEL")
       "Compact directory part"
   , Option "C" ["no-compact"]
       (NoArg (\opts -> opts { _compact = Nothing }))
       "NO Compact directory part"
   , Option "r" ["recursive"]
       (NoArg (\opts -> opts { _recursive = True }))
        "recursive path adding"
   , Option "f" ["filter"]
       (ReqArg (\value opts -> opts & pathFilters %~ (++ return (LiNameFilter value))) "FILTER_COMMAND")
        "Filter command for path"
   , Option "F" ["line-filter"]
       (ReqArg (\value opts -> opts & lineFilters %~ (++ return (LiNameFilter value))) "FILTER_COMMAND")
        "Filter command for line"
   , Option "R" ["no-recursive"]
       (NoArg (\opts -> opts { _recursive = False }))
        "No recursive path adding"
   , Option "e" ["ext", "extension"]
       (ReqArg (\value opts -> opts { _extension = value }) "EXTENSION")
        "Temporary file extension" ]


parseOptions :: Bool -> LiNameConfig -> [String] -> IO (Either String (LiNameConfig, [String]))
parseOptions allowEmpty conf argv = parseOptions' allowEmpty conf $ getOpt Permute options argv


parseOptions' :: Bool -> LiNameConfig -> ([LiNameConfig -> LiNameConfig], [String], [String]) -> IO (Either String (LiNameConfig, [String]))
parseOptions' False _ (_, [], es) = return $ Left $ usage es
parseOptions' _     c (o, n, [])  = return $ Right (foldl (flip id) c o, n)
parseOptions' _     _ (_, _, es)  = return $ Left $ usage es


usage :: [String] -> String
usage errs = concat errs ++ usageInfo header options
