module LiName.Undo (
  saveUndoInfo
) where


import LiName.Types

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.DateTime (formatDateTime, getCurrentTime)
import System.Directory (createDirectory, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))



saveUndoInfo :: [LiNameSuccess] -> IO ()
saveUndoInfo ss = do
    linameDir  <- (</> ".liname") <$> getHomeDirectory
    notExists <- not <$> doesDirectoryExist linameDir
    when notExists $ createDirectory linameDir
    n <- makeNewEntry
    writeFile (linameDir </> n) $ show ss


makeNewEntry :: IO (FilePath)
makeNewEntry = getCurrentTime >>= return . formatDateTime "%Y-%m-%d_%H:%M:%S.liname.undo"
