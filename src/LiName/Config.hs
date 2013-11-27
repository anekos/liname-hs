
{-# LANGUAGE ScopedTypeVariables #-}

module LiName.Config (
  getConf
) where


import LiName.Options
import LiName.Types

import Control.Applicative ((<$>))
import Control.Exception
import Control.Lens
import Data.Default (def)
import Data.Text (pack)
import Data.Yaml.Config
import Prelude hiding (lookup)
import System.Directory (getHomeDirectory)
import System.FilePath (combine)
import System.Posix.Files (getFdStatus, isNamedPipe)
import System.Posix.IO (stdInput)



getConf :: Bool -> [String] -> IO (Either String (LiNameConfig, [String]))
getConf allowEmpty as = do
    fileConf <- loadConfigFile
    fd <- getFdStatus stdInput
    pas <- if isNamedPipe fd then lines <$> getContents else return []
    parseOptions allowEmpty fileConf $ as ++ pas


loadConfigFile :: IO LiNameConfig
loadConfigFile = do
    home <- getHomeDirectory
    y <- load $ combine home ".liname.yaml"
    ec <- makeCommand' y "editor" editorCommandDefault
    tc <- makeCommand' y "trash" trashCommandDefault
    let cmded = def { _editorCommand = ec , _trashCommand = tc }
        opts = lookupDefault y (pack "options") []
    parsed <- parseOptions True cmded opts
    case parsed of
      (Right (conf, _)) -> return conf
      (Left e)          -> putStrLn e >> return cmded
  `catch`
    catchAndReturnDefault def


makeCommand :: LiNameCommand -> Config -> LiNameCommand
makeCommand d c = LiNameCommand { _path = lookupDefault c (pack "command") (d^.path)
                                , _args = lookupDefault c (pack "options") (d^.args)
                                , _placeHolder = lookupDefault c (pack "place_holder") (d^.placeHolder) }


makeCommand' :: Config -> String -> LiNameCommand -> IO LiNameCommand
makeCommand' y n d = withDefault d $ makeCommand d <$> subconfig y (pack n)


withDefault :: a -> IO a -> IO a
withDefault d a = a `catch` catchAndReturnDefault d


catchAndReturnDefault :: a -> SomeException -> IO a
catchAndReturnDefault d _ = return d
