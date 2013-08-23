
module LiName.Actions (
  doAction
) where


import LiName.Types
import LiName.Command

import Control.Applicative ((<$>))
import Control.Lens
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO.Error (catchIOError)
import System.FilePath.Posix (takeDirectory)
import Control.Exception (throwIO)
import Control.Monad (when)



doAction :: LiNameConfig -> LiNameAction -> LiNamePath -> IO (Either String ())
doAction _ (DoRename t) f
    | f == t               = return $ Right ()
    | otherwise            = msgCatch t $ checkExistingFile t $ createDirectoryIfMissing True (takeDirectory t) >> moveFile f t
doAction _ (DoCopy t) f
    | f == t               = return $ Right ()
    | otherwise            = msgCatch t $ checkExistingFile t $ createDirectoryIfMissing True (takeDirectory t) >> copyFile f t
doAction _ DoDelete f      = msgCatch f $ removeFile f
doAction c DoTrash  f      = msgCatch f $ fromExitCode <$> run (c^.trashCommand) f


fromExitCode :: ExitCode -> Either String ()
fromExitCode ExitSuccess      = Right ()
fromExitCode (ExitFailure x)  = Left $ "ExitCode: " ++ show x


checkExistingFile :: LiNamePath -> IO () -> IO ()
checkExistingFile fp a = do
    fe <- doesFileExist fp
    when fe $ throwIO $ userError $ "File already exists: " ++ fp
    de <- doesDirectoryExist fp
    when de $ throwIO $ userError $ "Directory already exists: " ++ fp
    a


moveFile :: FilePath -> FilePath -> IO ()
moveFile f t = renameFile f t `catchIOError` const (copyFile f t >> removeFile f)


msgCatch :: LiNamePath -> IO a -> IO (Either String ())
msgCatch fp a = do
    a
    return $ Right ()
  `catchIOError`
    (return . Left . (++ fp) . (++ ": ") . show)
