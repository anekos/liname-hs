
module LiName.Clean (
  clean
) where

import LiName.Types

import Data.List (nub)
import System.FilePath.Posix (takeDirectory)
import System.Posix.Directory (removeDirectory)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Control.Monad (when)
import Control.Applicative ((<$>))



clean :: [LiNamePath] -> IO ()
clean xs = do
    let dirs = nub $ map takeDirectory xs
    mapM_ rmDir dirs


rmDir :: LiNamePath -> IO ()
rmDir ""  = return ()
rmDir "/" = return ()
rmDir "." = return ()
rmDir p   = do
    exist <- doesDirectoryExist p
    when exist $ do
      empty <- null <$> getDirectoryContents p
      when empty $ print p >> removeDirectory p >> rmDir (takeDirectory p)
