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
                  | DoCopy
                  deriving Show


data LiNameEntry = LiNameEntry
                 { _key      :: LiNameKey
                 , _filepath :: LiNamePath
                 , _action   :: LiNameAction } deriving Show


data LiNameConfig = LiNameConfig
                  { _editor :: LCEditor }


instance Default LiNameConfig where
    def = LiNameConfig def


data LCEditor = LCEditor
              { _path         :: FilePath
              , _args         :: [String]
              , _placeHolder  :: String }

instance Default LCEditor where
  def = LCEditor "gvim" ["--nofork", "%"] "%"



makeLenses ''LCEditor
makeLenses ''LiNameConfig
makeLenses ''LiNameEntry



entryLine :: LiNameEntry -> String
entryLine (LiNameEntry key path action) = showAction action ++ showKey key ++ "\t" ++ showPath path
  where
    showKey (LiNameKey x)   = printf "%.4d" x
    showPath                = BU.toString . escape . BU.fromString
    showAction DoDelete     = "!!"
    showAction DoTrash      = "!"
    showAction DoCopy       = "="
    showAction _            = ""
