
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
import System.Posix.Files (getFileStatus, isDirectory, isRegularFile)
import System.IO.Error (catchIOError)



loadPath :: FilePath -> L [LiNamePath]
loadPath fp = do
    fs <- io $ getFileStatus fp
    case (isDirectory fs, isRegularFile fs) of
         (True, _) -> do xs <- loadDirectory fp -- FIXME `catchIO` const (return [])
                         return $ if null xs then [addDelim fp] else xs
         (_, True) -> return [fp]
         _         -> return []


loadPath' :: [FilePath] -> L [LiNamePath]
loadPath' fps = concat <$> mapM loadPath fps


ls :: FilePath -> L [FilePath]
ls dir = map (combine dir) <$> filter notDots <$> (io $ getDirectoryContents dir)


loadDirectory :: FilePath -> L [LiNamePath]
loadDirectory dir = concat <$> (ls dir >>= mapM loadPath)


makeSources:: [FilePath] -> [LiNameSource]
makeSources = zip (map LiNameKey [1..]) . map cleanPath


cleanPath :: LiNamePath -> LiNamePath
cleanPath fp = maybe fp unpack $ stripPrefix "./" $ pack fp
