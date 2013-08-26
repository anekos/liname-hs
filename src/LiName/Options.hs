
module LiName.Options (
  parseOptions
) where

import LiName.Types

import System.Console.GetOpt



header :: String
header = "Usage: liname [OPTION...] <PATH>..."


options :: [OptDescr (LiNameConfig -> LiNameConfig)]
options =
   [ Option "c" ["compact"]
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
