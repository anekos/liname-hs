{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module LiName.Types where

import Control.Lens
import Control.Monad.Reader
import Data.Default (def, Default)
import Data.Typeable


type LiNamePath = FilePath

data LiNameKey = LiNameKey Int deriving (Show, Read, Eq, Ord)


data LiNameAction = DoRename String
                  | DoDelete
                  | DoTrash
                  | DoCopy String
                  deriving (Show, Read, Eq)


type LiNameSource = (LiNameKey, LiNamePath)


data LiNameEntry = LiNameEntry { _entryKey :: LiNameKey, _action :: LiNameAction } deriving (Show, Read, Eq)


data LiNameUndoInfo = LiNameUndoInfo { _workDirectory :: LiNamePath, _commonPath :: LiNamePath, _logs :: [LiNameSuccess] } deriving (Show, Read, Eq)


data LiNameCommand = LiNameCommand
                   { _path         :: FilePath
                   , _args         :: [String]
                   , _placeHolder  :: String } deriving (Show, Read, Eq, Typeable)


data LiNameFilter = LiNameFilter String deriving (Show, Read, Eq, Typeable)


data LiNameSortType = SortByFileName
                    | SortByFilePath
                    | SortByFileNameI
                    | SortByFilePathI
                    | SortByModTime
                    | DontSort
                    | InvertedSort LiNameSortType deriving (Show, Read, Eq, Typeable)


data LiNameConfig = LiNameConfig
                  { _editorCommand :: LiNameCommand
                  , _trashCommand :: LiNameCommand
                  , _compact :: Maybe Int
                  , _sortType :: LiNameSortType
                  , _ignoreName :: Maybe String
                  , _ignorePath :: Maybe String
                  , _listFile :: Maybe String
                  , _pathFilters :: [LiNameFilter]
                  , _lineFilters :: [LiNameFilter]
                  , _recursive :: Bool
                  , _extension :: String } deriving (Show, Read, Eq, Typeable)


instance Default LiNameConfig where
    def = LiNameConfig { _editorCommand = editorCommandDefault
                       , _trashCommand = trashCommandDefault
                       , _compact = Nothing
                       , _sortType = DontSort
                       , _ignoreName = Nothing
                       , _ignorePath = Nothing
                       , _listFile = Nothing
                       , _pathFilters = []
                       , _lineFilters = []
                       , _recursive = True
                       , _extension = "liname" }


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
