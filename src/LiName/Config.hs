
{-# LANGUAGE ScopedTypeVariables #-}

module LiName.Config (
  loadConfigFile
) where


import LiName.Types

import Prelude hiding (catch)
import Control.Exception
import Control.Lens
import Data.Default (def)
import Data.Yaml.Config
import System.Directory (getHomeDirectory)
import System.FilePath (combine)
import System.IO.Error (catchIOError)
import Data.Text (pack)
import Control.Applicative ((<$>))


makeCommand :: LiNameCommand -> Config -> LiNameCommand
makeCommand d c = LiNameCommand { _path = lookupDefault c (pack "command") (d^.path)
                                , _args = lookupDefault c (pack "options") (d^.args)
                                , _placeHolder = lookupDefault c (pack "place_holder") (d^.placeHolder) }


loadConfigFile :: IO LiNameConfig
loadConfigFile = do
    home <- getHomeDirectory
    y <- load $ combine home ".liname.yaml"
    ec <- withDefault editorCommandDefault $ makeCommand editorCommandDefault <$> subconfig y (pack "editor")
    tc <- withDefault trashCommandDefault $ makeCommand editorCommandDefault <$> subconfig y (pack "trash")
    return $ LiNameConfig { _editorCommand = ec, _trashCommand = tc }
  `catchIOError`
    (return . const def)


withDefault :: a -> IO a -> IO a
withDefault d act = act `catch` catchAndReturnDefault d


catchAndReturnDefault :: a -> SomeException -> IO a
catchAndReturnDefault d e = return d
