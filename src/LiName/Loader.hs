
{-# LANGUAGE TemplateHaskell #-}

module LiName.Loader (
  loadPath
) where

import Control.Applicative
import LiName.Parsers
import LiName.Types
import System.Directory (getDirectoryContents)
import System.Posix.Files (getFileStatus, isDirectory)
import Data.List (concat, concatMap, filter)
import System.FilePath.Posix (joinPath)



loadPath :: FilePath -> IO [LiNamePath]
loadPath fp = do
    isDir <- isDirectory <$> getFileStatus fp
    if isDir then loadDirectory fp
             else return [fp]


ls :: FilePath -> IO [FilePath]
ls dir = map (joinPath . (dir:) . pure) <$> filter notDots <$> getDirectoryContents dir
  where
    notDots "."  = False
    notDots ".." = False
    notDots _    = True


loadDirectory :: FilePath -> IO [LiNamePath]
loadDirectory dir = concat <$> (ls dir >>= mapM loadPath)
