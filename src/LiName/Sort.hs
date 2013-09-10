
{-# LANGUAGE Rank2Types, ScopedTypeVariables, TupleSections #-}

module LiName.Sort (
  readSortType,
  sortPathList
) where

import LiName.Types


import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.Reader (ask)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (pack, toLower, Text)
import System.Directory (getModificationTime)
import System.FilePath.Posix (takeFileName)



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


sortPathList :: [LiNamePath] -> L [LiNamePath]
sortPathList xs = do
    st <- _sortType <$> ask
    io $ sortPathList' st xs


sortPathList' :: LiNameSortType -> [LiNamePath] -> IO [LiNamePath]
sortPathList' SortByFilePath    = sort' return
sortPathList' SortByFileName    = sort' (return . takeFileName)
sortPathList' SortByFilePathI   = sort' (return . ignoreCase)
sortPathList' SortByFileNameI   = sort' (return . ignoreCase . takeFileName)
sortPathList' SortByModTime     = sort' getModificationTime
sortPathList' DontSort          = sort' getModificationTime
sortPathList' (InvertedSort st) = liftM reverse . sortPathList' st


ignoreCase :: LiNamePath -> Text
ignoreCase = toLower . pack


sort' :: Ord a => (LiNamePath -> IO a) -> [LiNamePath] -> IO [LiNamePath]
sort' f xs = map fst . sortBy (compare `on` snd) <$> mapM (\x -> (x,) <$> f x) xs
