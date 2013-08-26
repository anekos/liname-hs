
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


parseOptions :: LiNameConfig -> [String] -> IO (Either String (LiNameConfig, [String]))
parseOptions conf argv =
    case getOpt Permute options argv of
       (_, [], es) -> usage es
       (o, n, [])  -> return $ Right (foldl (flip id) conf o, n)
       (_, _, es)  -> usage es
  where header = "Usage: liname [OPTION...] <PATH>..."
        usage errs = return $ Left $ concat errs ++ usageInfo header options
