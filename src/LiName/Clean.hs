
module LiName.Clean (
  clean
) where

import LiName.Types
import LiName.Utils

import Data.List (nub)
import System.FilePath.Posix (takeDirectory)
import System.Posix.Directory (removeDirectory)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Control.Monad (when)
import Control.Applicative ((<$>))



clean :: [LiNamePath] -> IO ()
clean xs = mapM_ rmDir $ nub $ map takeDirectory xs


rmDir :: LiNamePath -> IO ()
rmDir ""  = return ()
rmDir "/" = return ()
rmDir "." = return ()
rmDir p   = do
    exist <- doesDirectoryExist p
    when exist $ do
      empty <- null <$> filter notDots <$> getDirectoryContents p
      when empty $ removeDirectory p >> rmDir (takeDirectory p)
