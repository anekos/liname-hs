
module Main (main) where

import LiName.Main
import System.Environment (getArgs)



main :: IO ()
main = getArgs >>= getConf False >>= main'
