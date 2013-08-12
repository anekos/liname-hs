
module LiName.Types where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import           Text.ShellEscape (escape)
import           Text.Printf (printf)


type LinamePath = FilePath

data LinameKey = LinameKey Int deriving Show

data LinameAction = DoRename String
                  | DoDelete
                  | DoTrash
                  deriving Show

data LinameEntry = LinameEntry
                 { key      :: LinameKey
                 , path     :: LinamePath
                 , action   :: LinameAction } deriving Show


entryLine :: LinameEntry -> String
entryLine (LinameEntry key path action) = showAction action ++ showKey key ++ "\t" ++ showPath path
  where
    showKey (LinameKey x)   = printf "%.4d" x
    showPath                = BU.toString . escape . BU.fromString
    showAction DoDelete     = "!!"
    showAction DoTrash      = "!"
    showAction DoCopy       = "="
    showAction _            = ""
