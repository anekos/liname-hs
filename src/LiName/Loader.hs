
{-# LANGUAGE TemplateHaskell #-}

module LiName.Loader (
  loadPath,
  loadPath'
) where

import LiName.Parsers
import LiName.Types

import Control.Applicative ((<$>), pure)
import Data.List (concat, concatMap, filter)
import System.Directory (getDirectoryContents)
import System.FilePath.Posix (joinPath)
import System.Posix.Files (getFileStatus, isDirectory, isRegularFile)
import System.IO.Error (catchIOError)



loadPath :: FilePath -> IO [LiNamePath]
loadPath fp = do
    fs <- getFileStatus fp
    case (isDirectory fs, isRegularFile fs) of
         (True, _) -> loadDirectory fp `catchIOError` const (return [])
         (_, True) -> return [fp]
         _         -> return []


loadPath' :: [FilePath] -> IO [LiNamePath]
loadPath' fps = concat <$> mapM loadPath fps


ls :: FilePath -> IO [FilePath]
ls dir = map (joinPath . (dir:) . pure) <$> filter notDots <$> getDirectoryContents dir
  where
    notDots "."  = False
    notDots ".." = False
    notDots _    = True


loadDirectory :: FilePath -> IO [LiNamePath]
loadDirectory dir = concat <$> (ls dir >>= mapM loadPath)
