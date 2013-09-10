
{-# LANGUAGE OverloadedStrings #-}

module LiName.Loader (
  loadPath,
  loadPath',
  makeSources
) where

import LiName.Types
import LiName.Utils

import Control.Applicative ((<$>))
import Data.Text (pack, stripPrefix, unpack)
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import System.IO.Error (catchIOError)
import System.Posix.Files (getFileStatus, isDirectory, isRegularFile)



loadPath :: FilePath -> IO [LiNamePath]
loadPath fp = do
    fs <- getFileStatus fp
    case (isDirectory fs, isRegularFile fs) of
         (True, _) -> do xs <- loadDirectory fp `catchIOError` const (return [])
                         return $ if null xs then [addDelim fp] else xs
         (_, True) -> return [fp]
         _         -> return []


loadPath' :: [FilePath] -> IO [LiNamePath]
loadPath' fps = concat <$> mapM loadPath fps


ls :: FilePath -> IO [FilePath]
ls dir = map (combine dir) <$> filter notDots <$> getDirectoryContents dir


loadDirectory :: FilePath -> IO [LiNamePath]
loadDirectory dir = concat <$> (ls dir >>= mapM loadPath)


makeSources:: [FilePath] -> [LiNameSource]
makeSources = zip (map LiNameKey [1..]) . map cleanPath


cleanPath :: LiNamePath -> LiNamePath
cleanPath fp = maybe fp unpack $ stripPrefix "./" $ pack fp
