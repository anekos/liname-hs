
module LiName.Options (
  parseOptions
) where

import LiName.Sort
import LiName.Types
import LiName.Utils

import System.Console.GetOpt
import System.FilePath.Posix (takeFileName)



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
       (ReqArg (\value opts -> opts { _ignore = makeNotMatcher value . takeFileName }) "REGEXP")
       "Ignore pattern for file name"
   , Option "I" ["ignore-path"]
       (ReqArg (\value opts -> opts { _ignorePath = makeNotMatcher value }) "REGEXP")
       "Ignore pattern for path"
   , Option "c" ["compact"]
       (ReqArg (\value opts -> opts { _compact = Just $ read value }) "LEVEL")
       "Compact directory part"
   , Option "R" ["no-recursive"]
       (NoArg (\opts -> opts { _recursive = False }))
        "No recursive path adding" ]


parseOptions :: LiNameConfig -> [String] -> IO (Either String (LiNameConfig, [String]))
parseOptions conf argv = parseOptions' conf $ getOpt Permute options argv


parseOptions' :: LiNameConfig -> ([LiNameConfig -> LiNameConfig], [String], [String]) -> IO (Either String (LiNameConfig, [String]))
parseOptions' _ (_, [], es) = return $ Left $ usage es
parseOptions' c (o, n, [])  = return $ Right (foldl (flip id) c o, n)
parseOptions' _ (_, _, es)  = return $ Left $ usage es


usage :: [String] -> String
usage errs = concat errs ++ usageInfo header options
