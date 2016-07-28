module LiName.Undo (
  saveUndoInfo
) where


import LiName.Types

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (createDirectory, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))



saveUndoInfo :: LiNamePath -> LiNamePath -> [LiNameSuccess] -> IO ()
saveUndoInfo cwd cp ss = do
    linameDir  <- (</> ".liname") <$> getHomeDirectory
    notExists <- not <$> doesDirectoryExist linameDir
    when notExists $ createDirectory linameDir
    n <- makeNewEntry
    writeFile (linameDir </> n) $ show $ LiNameUndoInfo cwd cp ss


makeNewEntry :: IO FilePath
makeNewEntry = do
  t <- getCurrentTime
  return $ formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S.liname.undo" t
