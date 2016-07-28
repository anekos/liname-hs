
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module LiName.Config (
  getConf,
  loadConfigFile
) where


import LiName.Options
import LiName.Types

import Control.Applicative ((<$>))
import Control.Exception
import Control.Lens
import Data.Default (def)
import Data.Text (pack, Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON, (.:), (.:?), (.!=))
import Data.Yaml.Config (loadYamlSettings, ignoreEnv)
import Prelude hiding (lookup)
import System.Directory (getHomeDirectory)
import System.FilePath (combine)
import System.Posix.Files (getFdStatus, isNamedPipe)
import System.Posix.IO (stdInput)


data LiNameYamlConfig = LiNameYamlConfig { _yEditorCommand :: LiNameCommand
                                         , _yTrashCommand :: LiNameCommand
                                         , _yOptoins :: [String]}

instance FromJSON LiNameCommand where
  parseJSON (Y.Object v) =
    LiNameCommand <$>
      v .: "command" <*>
      v .: "options" <*>
      v .: "place_holder"


instance FromJSON LiNameYamlConfig where
  parseJSON (Y.Object v) =
    LiNameYamlConfig <$>
      v .:? "editor" .!= editorCommandDefault <*>
      v .:? "trash" .!= trashCommandDefault <*>
      v .:? "options" .!= []
  parseJSON _ = fail "Expected Object for Config value"


fromYConfig :: LiNameYamlConfig -> IO LiNameConfig
fromYConfig (LiNameYamlConfig e t o) = do
  let cmded = def { _editorCommand = e, _trashCommand = t }
  parsed <- parseOptions True cmded o
  case parsed of
       (Right (conf, _)) -> return conf
       (Left e)          -> putStrLn e >> return cmded


getConf :: Bool -> [String] -> IO (Either String (LiNameConfig, [String]))
getConf allowEmpty as = do
    fileConf <- loadConfigFile
    fd <- getFdStatus stdInput
    pas <- if isNamedPipe fd then lines <$> getContents else return []
    parseOptions allowEmpty fileConf $ as ++ pas


loadConfigFile :: IO LiNameConfig
loadConfigFile = do
    home <- getHomeDirectory
    let y = combine home ".liname.yaml"
    loadYamlSettings [y] [] ignoreEnv >>= fromYConfig
  `catch`
    catchAndReturnDefault def


withDefault :: a -> IO a -> IO a
withDefault d a = a `catch` catchAndReturnDefault d


catchAndReturnDefault :: a -> SomeException -> IO a
catchAndReturnDefault d _ = return d
