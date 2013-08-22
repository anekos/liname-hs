
module LiName.Actions (
  doAction
) where


import LiName.Types
import LiName.Command

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad (void)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO.Error (catchIOError)
import System.Posix.Files (removeLink, rename)
import System.FilePath.Posix (takeDirectory)



doAction :: LiNameConfig -> LiNameAction -> LiNamePath -> IO (Either String ())
doAction conf (DoRename t) f
    | f == t               = return $ Right ()
    | otherwise            = msgCatch t $ createDirectoryIfMissing True (takeDirectory t) >> rename f t
doAction _ (DoCopy t) f
    | f == t               = return $ Right ()
    | otherwise            = msgCatch t $ createDirectoryIfMissing True (takeDirectory t) >> copyFile f t
doAction _ DoDelete f      = msgCatch f $ removeLink f
doAction c DoTrash  f      = msgCatch f $ fromExitCode <$> run (c^.trashCommand) f


fromExitCode :: ExitCode -> Either String ()
fromExitCode ExitSuccess      = Right ()
fromExitCode (ExitFailure x)  = Left $ "ExitCode: " ++ show x


msgCatch :: LiNamePath -> IO a -> IO (Either String ())
msgCatch fp act = do
    act
    return $ Right ()
  `catchIOError`
    (return . Left . (++ fp) . (++ ": ") . show)
