
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
import System.FilePath.Posix (takeFileName)
import System.IO (hPrint, stderr)
import System.IO.Error (catchIOError)
import System.Posix.Files (getFileStatus, isDirectory, isRegularFile)



loadPath :: [FilePath] -> L [LiNamePath]
loadPath fps = do
    matchPath <- (matcher id . _ignorePath) <$> ask
    matchName <- (matcher takeFileName . _ignoreName) <$> ask
    filter matchName . filter matchPath . map cleanPath . concat <$> mapM loadPath' fps
  where
    matcher _ Nothing  = const True
    matcher f (Just v) = makeNotMatcher v . f


loadPath' :: FilePath -> L [LiNamePath]
loadPath' fp = do
    rec <- _recursive <$> ask
    -- getFileStatus fails, if the path does not exist.
    mfs <- io $ getFileStatus' `catchIOError` putError
    case mfs of
      Nothing -> return []
      Just fs ->
        case (rec, isDirectory fs, isRegularFile fs) of
             (False, _, _) -> return [fp]
             (_, True, _)  -> do xs <- loadDirectory fp
                                 return $ if null xs then [addDelim fp] else xs
             (_, _, True)  -> return [fp]
             _             -> return []
  where
    getFileStatus' = fmap Just (getFileStatus fp)
    putError e = hPrint stderr e >> return Nothing


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
