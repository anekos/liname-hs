
module LiName.Command (
  edit,
  run
) where

import LiName.Types

import Control.Applicative ((<$>))
import Control.Lens
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (waitForProcess, runProcess)



run :: LiNameCommand -> String -> IO ExitCode
run cmd arg = do
    let as = genArgs cmd arg
    runProcess (cmd^.path) as Nothing Nothing Nothing Nothing Nothing >>= waitForProcess


edit :: LiNameCommand -> [String] -> IO [String]
edit cmd contents = do
    fn <- makeTempFile contents
    run cmd fn
    ls <- lines <$> readFile fn
    removeFile fn
    return ls


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
replace f as t = map (\x -> if x == f then t else x) as
