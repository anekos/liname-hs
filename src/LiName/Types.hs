
module LiName.Types where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import           Text.ShellEscape (escape)
import           Text.Printf (printf)
import           Data.Default (def, Default)


type LiNamePath = FilePath

data LiNameKey = LiNameKey Int deriving (Show, Eq, Ord)


data LiNameAction = DoRename String
                  | DoDelete
                  | DoTrash
                  | DoCopy
                  deriving Show


data LiNameEntry = LiNameEntry
                 { key      :: LiNameKey
                 , path     :: LiNamePath
                 , action   :: LiNameAction } deriving Show


data LiNameConfig = LiNameConfig
                  { editor :: LCEditor }

instance Default LiNameConfig where
    def = LiNameConfig def


data LCEditor = LCEditor
              { lcePath         :: FilePath
              , lceArgs         :: [String]
              , lcePlaceHolder  :: String }

instance Default LCEditor where
  def = LCEditor "gvim" ["--nofork", "%"] "%"



entryLine :: LiNameEntry -> String
entryLine (LiNameEntry key path action) = showAction action ++ showKey key ++ "\t" ++ showPath path
  where
    showKey (LiNameKey x)   = printf "%.4d" x
    showPath                = BU.toString . escape . BU.fromString
    showAction DoDelete     = "!!"
    showAction DoTrash      = "!"
    showAction DoCopy       = "="
    showAction _            = ""
