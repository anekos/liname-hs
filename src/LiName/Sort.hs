
{-# LANGUAGE Rank2Types, ScopedTypeVariables, TupleSections #-}

module LiName.Sort (
  readSortType,
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



-- FIXME IOError should occurs for unknow type.
readSortType :: String -> LiNameSortType
readSortType ('i':xs) = InvertedSort $ readSortType xs
readSortType "m"      = SortByModTime
readSortType "n"      = SortByFileName
readSortType "p"      = SortByFilePath
readSortType "N"      = SortByFileNameI
readSortType "P"      = SortByFilePathI
readSortType "-"      = DontSort
readSortType _        = error "Unknow sort type"


sortPathList :: LiNameSortType -> [LiNamePath] -> L [LiNamePath]
sortPathList SortByFilePath    = io . sort' return
sortPathList SortByFileName    = io . sort' (return . takeFileName)
sortPathList SortByFilePathI   = io . sort' (return . ignoreCase)
sortPathList SortByFileNameI   = io . sort' (return . ignoreCase . takeFileName)
sortPathList SortByModTime     = io . sort' getModificationTime
sortPathList DontSort          = io . sort' getModificationTime
sortPathList (InvertedSort st) = liftM reverse . sortPathList st


ignoreCase :: LiNamePath -> Text
ignoreCase = toLower . pack


sort' :: Ord a => (LiNamePath -> IO a) -> [LiNamePath] -> IO [LiNamePath]
sort' f xs = map fst . sortBy (compare `on` snd) <$> mapM (\x -> (x,) <$> f x) xs
