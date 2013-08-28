
{-# LANGUAGE Rank2Types, ScopedTypeVariables, TupleSections #-}

module LiName.Sort (
  sortPathList
) where

import LiName.Types
import System.FilePath.Posix (takeFileName)
import Data.Function (on)
import Data.List (sortBy)
import System.Directory (getModificationTime)
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.Text (pack, toLower, Text)


sortPathList :: LiNameSortType -> [LiNamePath] -> IO [LiNamePath]
sortPathList SortByFilePath    = sort' return
sortPathList SortByFileName    = sort' (return . takeFileName)
sortPathList SortByFilePathI   = sort' (return . ignoreCase)
sortPathList SortByFileNameI   = sort' (return . ignoreCase . takeFileName)
sortPathList SortByModTime     = sort' getModificationTime
sortPathList DontSort          = sort' getModificationTime
sortPathList (InvertedSort st) = liftM reverse . sortPathList st


ignoreCase :: LiNamePath -> Text
ignoreCase = toLower . pack


sort' :: Ord a => (LiNamePath -> IO a) -> [LiNamePath] -> IO [LiNamePath]
sort' f xs = map fst . sortBy (compare `on` snd) <$> mapM (\x -> (x,) <$> f x) xs
