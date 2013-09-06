
module Main (main) where

import LiName.Main



main :: IO ()
main = getConf >>= main'
