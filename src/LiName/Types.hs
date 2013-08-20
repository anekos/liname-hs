{-# LANGUAGE TemplateHaskell #-}

module LiName.Types where

import           Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import           Data.Default (def, Default)
import           Text.Printf (printf)
import           Text.ShellEscape (escape)


type LiNamePath = FilePath

data LiNameKey = LiNameKey Int deriving (Show, Eq, Ord)


data LiNameAction = DoRename String
                  | DoDelete
                  | DoTrash
                  | DoCopy String
                  deriving (Show, Eq)


type LiNameSource = (LiNameKey, LiNamePath)


data LiNameEntry = LiNameEntry { _entryKey :: LiNameKey, _action :: LiNameAction } deriving (Show, Eq)


data LiNameCommand = LiNameCommand
                   { _path         :: FilePath
                   , _args         :: [String]
                   , _placeHolder  :: String }


data LiNameConfig = LiNameConfig
                  { _editorCommand :: LiNameCommand
                  , _trashCommand :: LiNameCommand }


instance Default LiNameConfig where
    def = LiNameConfig { _editorCommand = LiNameCommand "gvim" ["--nofork", "%"] "%"
                       , _trashCommand = LiNameCommand "gvim" ["--nofork", "%"] "%" }




makeLenses ''LiNameCommand
makeLenses ''LiNameConfig
makeLenses ''LiNameEntry
