module LiName.Undo (
  saveUndoInfo
) where


import LiName.Types

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.DateTime (formatDateTime, getCurrentTime)
import System.Directory (createDirectory, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))



saveUndoInfo :: LiNamePath -> [LiNameSuccess] -> IO ()
saveUndoInfo cp ss = do
    linameDir  <- (</> ".liname") <$> getHomeDirectory
    notExists <- not <$> doesDirectoryExist linameDir
    when notExists $ createDirectory linameDir
    n <- makeNewEntry
    writeFile (linameDir </> n) $ show $ LiNameUndoInfo cp ss


makeNewEntry :: IO FilePath
makeNewEntry = formatDateTime "%Y-%m-%d_%H:%M:%S.liname.undo" <$> getCurrentTime
