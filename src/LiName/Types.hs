{-# LANGUAGE TemplateHaskell #-}

module LiName.Types where

import Control.Lens
import Data.Default (def, Default)


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
                   , _placeHolder  :: String } deriving Show


data LiNameConfig = LiNameConfig
                  { _editorCommand :: LiNameCommand
                  , _trashCommand :: LiNameCommand } deriving Show


instance Default LiNameConfig where
    def = LiNameConfig { _editorCommand = editorCommandDefault
                       , _trashCommand = trashCommandDefault }


editorCommandDefault, trashCommandDefault :: LiNameCommand
editorCommandDefault = LiNameCommand "gvim" ["--nofork", "%"] "%"
trashCommandDefault  = LiNameCommand "false" [] "%"


makeLenses ''LiNameCommand
makeLenses ''LiNameConfig
makeLenses ''LiNameEntry
