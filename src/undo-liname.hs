
{-# LANGUAGE ScopedTypeVariables, TupleSections, OverloadedStrings #-}

import LiName.Actions
import LiName.Config
import LiName.Result
import LiName.Types

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Control.Monad.Reader (runReaderT)
import Data.List (sort)
import System.Directory (doesFileExist, getDirectoryContents, getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>), takeFileName)
import Text.Printf (printf)


main :: IO ()
main = do
  econf <- getArgs >>= getConf True
  case econf of
    (Right (conf, as)) -> runReaderT (main' as) conf
    (Left err)         -> putStrLn err


main' :: [String] -> L ()
main' []    = io $ getUndoInfoFiles >>= mapM_ (putStrLn . undoInfoLine) . zip [0..]
main' (n:_) = do
    LiNameUndoInfo wd cp logs <- io $ read <$> (getUndoInfoFiles >>= readFile . (!! read n) . reverse . sort)
    let cp' = wd </> cp
    mapM (process cp') logs >>= io . putResult "target" (length logs)


process :: LiNamePath -> LiNameSuccess -> L LiNameResult
process common s@(_, fp) = do
    r <- undoAction common s
    case r of
      Right ()  -> return $ Right s
      Left err' -> return $ Left (fp, err')


getUndoInfoFiles :: IO [String]
getUndoInfoFiles = do
    dir <- (</> ".liname") <$> getHomeDirectory
    (reverse . sort) <$> getDirectoryContents dir >>= mapM (return . (dir </>)) >>= filterM doesFileExist


undoInfoLine :: (Int, FilePath) -> String
undoInfoLine (i, fp) = printf "%4d = %s" i $ takeFileName fp
