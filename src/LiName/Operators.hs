
module LiName.Operators where

import LiName.Types
import Data.List (find)
import Data.Maybe (fromMaybe)



merge :: [LiNameEntry] -> [LiNameEntry]
merge xs = map f xs
  where
    f x = fromMaybe x $ find (\y -> key y == key x) xs
