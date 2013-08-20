
module LiName.Command (
  edit,
  run
) where

import LiName.Types

import Control.Applicative ((<$>))
import Control.Lens
import Data.Default (def)
import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode(..))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (waitForProcess, runProcess)



run :: LiNameCommand -> String -> IO ExitCode
run cmd arg = do
    let args = genArgs cmd arg
    runProcess (cmd^.path) args Nothing Nothing Nothing Nothing Nothing >>= waitForProcess


edit :: LiNameCommand -> [String] -> IO [String]
edit cmd contents = do
    fn <- makeTempFile contents
    run cmd fn
    lines <$> readFile fn


makeTempFile :: [String] -> IO String
makeTempFile cs = do
    dir <- getTemporaryDirectory
    (fn, h) <- openTempFile dir "linameXXXXX.txt"
    hPutStr h $ unlines cs
    hClose h
    return fn


genArgs :: LiNameCommand -> String -> [String]
genArgs conf = replace (conf^.placeHolder) (conf^.args)


replace :: String -> [String] -> String -> [String]
replace from args to = map (\x -> if x == from then to else x) args
