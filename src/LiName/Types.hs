{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TypeSynonymInstances, CPP, DeriveDataTypeable #-}

module LiName.Types where

import Control.Applicative
import Data.Monoid
import Control.Lens
import Data.Default (def, Default)
import Control.Monad.Reader
import Control.Monad.State
import Data.Typeable


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


data LiNameSortType = SortByFileName
                    | SortByFilePath
                    | SortByFileNameI
                    | SortByFilePathI
                    | SortByModTime
                    | DontSort
                    | InvertedSort LiNameSortType deriving Show


data LiNameConfig = LiNameConfig
                  { _editorCommand :: LiNameCommand
                  , _trashCommand :: LiNameCommand
                  , _compact :: Maybe Int
                  , _sortType :: LiNameSortType } deriving Show


instance Default LiNameConfig where
    def = LiNameConfig { _editorCommand = editorCommandDefault
                       , _trashCommand = trashCommandDefault
                       , _compact = Nothing
                       , _sortType = DontSort }


type LiNameSuccess = (LiNameEntry, LiNamePath)
type LiNameFail = (String, String)  -- (SourceLine, ErrorDescription)
type LiNameResult = Either LiNameFail LiNameSuccess


editorCommandDefault, trashCommandDefault :: LiNameCommand
editorCommandDefault = LiNameCommand "gvim" ["--nofork", "%"] "%"
trashCommandDefault  = LiNameCommand "false" [] "%"


type L a = ReaderT LiNameConfig IO a

io :: MonadIO m => IO a -> m a
io = liftIO



makeLenses ''LiNameCommand
makeLenses ''LiNameConfig
makeLenses ''LiNameEntry
