
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
import Text.Regex
import Data.Maybe (isJust)



loadPath :: [FilePath] -> L [LiNamePath]
loadPath fps = do
    reMatcher <- maybe (const True) makeMatcher . _ignore <$> ask
    filter reMatcher . map cleanPath . concat <$> mapM loadPath' fps


loadPath' :: FilePath -> L [LiNamePath]
loadPath' fp = do
    fs <- io $ getFileStatus fp
    case (isDirectory fs, isRegularFile fs) of
         (True, _) -> do xs <- loadDirectory fp
                         return $ if null xs then [addDelim fp] else xs
         (_, True) -> return [fp]
         _         -> return []


makeMatcher :: String -> String -> Bool
makeMatcher s = let re = mkRegex s
                in  not . isJust . matchRegex re


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
