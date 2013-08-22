
{-# LANGUAGE ScopedTypeVariables #-}

import LiName.Actions
import LiName.Command
import LiName.Config
import LiName.Loader
import LiName.Parsers
import LiName.Types

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad (forM, forM_)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Default (def)
import Data.Either (rights)
import Data.Map (Map, fromList)
import Data.Map.Lazy (lookup)
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.ShellEscape (escape)



sourceLine :: LiNameSource -> String
sourceLine ((LiNameKey key), path) = printf "%.4d\t%s" key $ toString $ escape $ fromString path


process :: LiNameConfig -> Map LiNameKey LiNamePath -> LiNameEntry -> IO (Either String ())
process conf sm (LiNameEntry {_entryKey = key, _action = act })
    | Just fp <- lookup key sm = doAction conf act fp
    | otherwise                = return $ Left $ "Not found key: " ++ show key


main = do
    let conf = def :: LiNameConfig
    ss :: [LiNameSource] <- makeSources <$> (loadPath' =<< getArgs)
    es' <- map (parseEntry "<TEMP>") <$> edit (def^.editorCommand) (map sourceLine ss)
    results <- forM (rights es') $ process conf (fromList ss)
    forM_ results print
