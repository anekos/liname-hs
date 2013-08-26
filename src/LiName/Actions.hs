
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
import System.FilePath.Posix ((</>), takeDirectory)
import Control.Exception (throwIO)
import Control.Monad (when)



doAction :: LiNameConfig -> LiNamePath -> LiNameAction -> LiNamePath -> IO (Either String ())
doAction _ cp (DoRename t) f  = doRename (cp </> f) (cp </> t)
doAction _ cp (DoCopy t) f    = doCopy (cp </> f) (cp </> t)
doAction _ cp DoDelete f      = msgCatch f $ removeFile (cp </> f)
doAction c cp DoTrash  f      = msgCatch f $ fromExitCode <$> run (c^.trashCommand) (cp </> f)


doRename :: LiNamePath -> LiNamePath -> IO (Either String ())
doRename f t
    | f == t    = return $ Right ()
    | otherwise = msgCatch t $ checkExistingFile t $ createDirectoryIfMissing True (takeDirectory t) >> moveFile f t


doCopy :: LiNamePath -> LiNamePath -> IO (Either String ())
doCopy f t
    | f == t    = return $ Right ()
    | otherwise = msgCatch t $ checkExistingFile t $ createDirectoryIfMissing True (takeDirectory t) >> copyFile f t


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
