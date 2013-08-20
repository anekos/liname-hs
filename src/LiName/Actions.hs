
module LiName.Actions (
  doAction
) where


import LiName.Types
import LiName.Command

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad (void)
import System.Directory (copyFile)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Error (catchIOError)
import System.Posix.Files (removeLink, rename)



doAction :: LiNameConfig -> LiNameEntry -> IO Bool
doAction c e = doAction' c (e^.filepath) (e^.action)


doAction' :: LiNameConfig -> LiNamePath -> LiNameAction -> IO Bool
doAction' _ f (DoRename t) = eqOr f t <$> (boolCatch $ rename f t)
doAction' _ f (DoCopy t)   = eqOr f t <$> (boolCatch $ copyFile f t)
doAction' _ f DoDelete     = boolCatch $ removeLink f
doAction' c f DoTrash      = boolCatch $ (== ExitSuccess) <$> run (c^.trashCommand) f


eqOr :: Eq a => a -> a -> Bool -> Bool
eqOr x y = ((x == y) ||)


boolCatch :: IO a -> IO Bool
boolCatch act = act >> return True `catchIOError` const (return False)
