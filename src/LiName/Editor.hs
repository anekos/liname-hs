
module LiName.Editor (
  edit
) where

import LiName.Types

import System.Process
import System.Exit (ExitCode(..))
import System.Directory (getTemporaryDirectory)
import System.IO (hClose, hPutStr, openTempFile)
import Data.Default (def)



edit :: LCEditor -> [String] -> IO [String]
edit conf contents = do
    fn <- makeTempFile contents
    runEditor (lcePath conf) $ editorArguments conf fn
    readFile fn >>= return . lines


runEditor :: FilePath -> [String] -> IO ExitCode
runEditor path args = runProcess path args Nothing Nothing Nothing Nothing Nothing >>= waitForProcess


makeTempFile :: [String] -> IO String
makeTempFile cs = do
    dir <- getTemporaryDirectory
    (fn, h) <- openTempFile dir "linameXXXXX.txt"
    hPutStr h $ unlines cs
    hClose h
    return fn


replace :: String -> [String] -> String -> [String]
replace from args to = map (\x -> if x == from then to else x) args


editorArguments :: LCEditor -> String -> [String]
editorArguments conf = replace (lcePlaceHolder conf) $ lceArgs conf
