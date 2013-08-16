
{-# LANGUAGE TemplateHaskell #-}

module LiName.Loader (
) where

import Control.Applicative
import LiName.Parsers
import LiName.Types
import System.Directory (getDirectoryContents)
import System.Posix.Files (getFileStatus, isDirectory)
import Data.List (concatMap)


loadPath :: FilePath -> IO [LiNamePath]
loadPath fp = do
    isDir <- isDirectory <$> getFileStatus fp
    if isDir then loadDirectory fp
             else return [fp]


loadDirectory :: FilePath -> IO [LiNamePath]
loadDirectory dir = do
    cs <- getDirectoryContents dir
    return []
