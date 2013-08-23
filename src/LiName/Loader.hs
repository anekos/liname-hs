
module LiName.Loader (
  loadPath,
  loadPath',
  makeSources
) where

import LiName.Types
import LiName.Utils

import Control.Applicative ((<$>))
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
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
ls dir = map (combine dir) <$> filter notDots <$> getDirectoryContents dir


loadDirectory :: FilePath -> IO [LiNamePath]
loadDirectory dir = concat <$> (ls dir >>= mapM loadPath)


makeSources:: [FilePath] -> [LiNameSource]
makeSources= zip $ map LiNameKey [0..]
