
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
import Data.Text (pack)
import Control.Applicative ((<$>))



loadConfigFile :: IO LiNameConfig
loadConfigFile = do
    home <- getHomeDirectory
    y <- load $ combine home ".liname.yaml"
    ec <- makeCommand' y "editor" editorCommandDefault
    tc <- makeCommand' y "trash" trashCommandDefault
    return $ LiNameConfig { _editorCommand = ec, _trashCommand = tc }
  `catch`
    catchAndReturnDefault def


makeCommand :: LiNameCommand -> Config -> LiNameCommand
makeCommand d c = LiNameCommand { _path = lookupDefault c (pack "command") (d^.path)
                                , _args = lookupDefault c (pack "options") (d^.args)
                                , _placeHolder = lookupDefault c (pack "place_holder") (d^.placeHolder) }


makeCommand' :: Config -> String -> LiNameCommand -> IO LiNameCommand
makeCommand' y n d = withDefault d $ makeCommand d <$> subconfig y (pack n)


withDefault :: a -> IO a -> IO a
withDefault d act = act `catch` catchAndReturnDefault d


catchAndReturnDefault :: a -> SomeException -> IO a
catchAndReturnDefault d e = return d
