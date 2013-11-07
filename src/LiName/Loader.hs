
{-# LANGUAGE OverloadedStrings #-}

module LiName.Loader (
  loadPath,
  makeSources
) where

import LiName.Types
import LiName.Utils

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Data.Text (pack, stripPrefix, unpack)
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import System.IO.Error (catchIOError)
import System.Posix.Files (getFileStatus, isDirectory, isRegularFile)



loadPath :: [FilePath] -> L [LiNamePath]
loadPath fps = do
    pathMatcher <- _ignorePath <$> ask
    nameMatcher <- _ignore <$> ask
    filter nameMatcher . filter pathMatcher . map cleanPath . concat <$> mapM loadPath' fps


loadPath' :: FilePath -> L [LiNamePath]
loadPath' fp = do
    -- getFileStatus fails, if the path does not exist.
    mfs <- io $ fmap Just (getFileStatus fp) `catchIOError` const (return Nothing)
    case mfs of
      Nothing -> return []
      Just fs ->
        case (isDirectory fs, isRegularFile fs) of
             (True, _) -> do xs <- loadDirectory fp
                             return $ if null xs then [addDelim fp] else xs
             (_, True) -> return [fp]
             _         -> return []


ls :: FilePath -> L [FilePath]
ls dir = map (combine dir) <$> filter notDots <$> getDir
  where
    getDir = io $ getDirectoryContents dir `catchIOError` const (return [])


loadDirectory :: FilePath -> L [LiNamePath]
loadDirectory dir = concat <$> (ls dir >>= mapM loadPath')


makeSources:: [FilePath] -> [LiNameSource]
makeSources = zip (map LiNameKey [1..])


cleanPath :: LiNamePath -> LiNamePath
cleanPath fp = maybe fp unpack $ stripPrefix "./" $ pack fp
