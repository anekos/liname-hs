
module LiName.Result (
  putResult
) where

import LiName.Types
import LiName.Utils

import Control.Monad (forM_, unless)
import Data.Either (lefts, rights)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)


putResult :: String -> Int -> [LiNameResult] -> IO ()
putResult src target rs = do
    let fails = lefts rs
    putStrLn "Results"
    putStrLn $ printf (indent "%-7s      %4d") (src ++ ":") target
    putStrLn $ printf (indent "try:         %4d") $ length rs
    putStrLn $ printf (indent "success:     %4d") $ length $ rights rs
    putStrLn $ printf (indent "fail:        %4d") $ length fails
    unless (null fails) $ do
      hPutStrLn stderr "Fails"
      forM_ fails $ \(line, err) -> hPutStrLn stderr $ indent line ++ "\n" ++ indent ('\t' : unbreak err)
