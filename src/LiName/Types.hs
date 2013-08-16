
module LiName.Types where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import           Text.ShellEscape (escape)
import           Text.Printf (printf)
import           Data.Default (def, Default)


type LinamePath = FilePath

data LinameKey = LinameKey Int deriving (Show, Eq, Ord)

data LinameAction = DoRename String
                  | DoDelete
                  | DoTrash
                  | DoCopy
                  deriving Show

data LinameEntry = LinameEntry
                 { key      :: LinameKey
                 , path     :: LinamePath
                 , action   :: LinameAction } deriving Show


data LiNameConfig = LiNameConfig
                  { editor :: LCEditor }


data LCEditor = LCEditor
              { lcePath         :: FilePath
              , lceArgs         :: [String]
              , lcePlaceHolder  :: String }

instance Default LCEditor where
  def = LCEditor "gvim" ["--nofork", "%"] "%"



entryLine :: LinameEntry -> String
entryLine (LinameEntry key path action) = showAction action ++ showKey key ++ "\t" ++ showPath path
  where
    showKey (LinameKey x)   = printf "%.4d" x
    showPath                = BU.toString . escape . BU.fromString
    showAction DoDelete     = "!!"
    showAction DoTrash      = "!"
    showAction DoCopy       = "="
    showAction _            = ""
