
module Main (main) where

import LiName.Main



main :: IO ()
main = do getConf >>= print
          getConf >>= main'
