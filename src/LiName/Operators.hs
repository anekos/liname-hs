
{-# LANGUAGE TemplateHaskell #-}

module LiName.Operators where

import LiName.Types

import Control.Lens
import Data.List (find)
import Data.Maybe (fromMaybe)



merge :: [LiNameEntry] -> [LiNameEntry]
merge xs = map f xs
  where
    f x = fromMaybe x $ find (\y -> y^.key == x^.key) xs
