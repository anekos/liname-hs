
{-# LANGUAGE TemplateHaskell #-}

module LiName.Operators where

import LiName.Types
import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Lens



merge :: [LiNameEntry] -> [LiNameEntry]
merge xs = map f xs
  where
    f x = fromMaybe x $ find (\y -> y^.key == x^.key) xs
