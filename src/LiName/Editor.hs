
{-# LANGUAGE TemplateHaskell #-}

module LiName.Editor (
  edit
) where

import LiName.Types

import Data.Default (def)
import Control.Lens
import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode(..))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process



edit :: LCEditor -> [String] -> IO [String]
edit conf contents = do
    fn <- makeTempFile contents
    runEditor (conf^.path) $ editorArguments conf fn
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
editorArguments conf = replace (conf^.placeHolder) (conf^.args)
