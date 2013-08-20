
module LiName.Actions (
  doAction
) where


import LiName.Types
import LiName.Command

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad (void)
import System.Directory (copyFile)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO.Error (catchIOError)
import System.Posix.Files (removeLink, rename)



doAction :: LiNameConfig -> FilePath -> LiNameEntry -> IO (Either String ())
doAction c fp e = doAction' c fp (e^.action)


doAction' :: LiNameConfig -> LiNamePath -> LiNameAction -> IO (Either String ())
doAction' _ f (DoRename t)
    | f == t               = return $ Right ()
    | otherwise            = msgCatch $ rename f t
doAction' _ f (DoCopy t)
    | f == t               = return $ Right ()
    | otherwise            = msgCatch $ copyFile f t
doAction' _ f DoDelete     = msgCatch $ removeLink f
doAction' c f DoTrash      = msgCatch $ fromExitCode <$> run (c^.trashCommand) f


fromExitCode :: ExitCode -> Either String ()
fromExitCode ExitSuccess      = Right ()
fromExitCode (ExitFailure x)  = Left $ "ExitCode: " ++ show x


msgCatch :: IO a -> IO (Either String ())
msgCatch act = do
    act
    return $ Right ()
  `catchIOError`
    (return . Left . show)
