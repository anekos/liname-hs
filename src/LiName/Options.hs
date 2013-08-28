
module LiName.Options (
  parseOptions
) where

import LiName.Types

import System.Console.GetOpt



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
   ]


parseOptions :: LiNameConfig -> [String] -> IO (Either String (LiNameConfig, [String]))
parseOptions conf argv =
    case getOpt Permute options argv of
       (_, [], es) -> usage es
       (o, n, [])  -> return $ Right (foldl (flip id) conf o, n)
       (_, _, es)  -> usage es
  where header = "Usage: liname [OPTION...] <PATH>..."
        usage errs = return $ Left $ concat errs ++ usageInfo header options


-- FIXME Error should occurs for unknow type.
readSortType :: String -> LiNameSortType
readSortType ('i':xs) = InvertedSort $ readSortType xs
readSortType "m"      = SortByModTime
readSortType "n"      = SortByFileName
readSortType "p"      = SortByFilePath
readSortType "N"      = SortByFileNameI
readSortType "P"      = SortByFilePathI
readSortType "-"      = DontSort
readSortType _        = DontSort
