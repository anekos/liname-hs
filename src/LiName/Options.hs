
module LiName.Options (
  parseOptions
) where

import LiName.Types

import System.Console.GetOpt



options :: [OptDescr (LiNameConfig -> LiNameConfig)]
options =
   [ Option "s" ["squash"]
       (ReqArg (\value opts -> opts { _squash = Just $ read value }) "LEVEL")
       "Squash directory part"
   ]


parseOptions :: LiNameConfig -> [String] -> IO (LiNameConfig, [String])
parseOptions conf argv =
    case getOpt Permute options argv of
       (o, n, [])   -> return (foldl (flip id) conf o, n)
       (_, _, errs) -> ioError $ userError (concat errs ++ usageInfo header options)
  where header = "Usage: liname [OPTION...] <PATH>..."
