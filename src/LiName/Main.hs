
{-# LANGUAGE ScopedTypeVariables, TupleSections, OverloadedStrings #-}

module LiName.Main (main', getConf) where


import LiName.Actions
import LiName.Clean
import LiName.Command
import LiName.Config
import LiName.Filter
import LiName.Loader
import LiName.Parsers
import LiName.Path
import LiName.Result
import LiName.Sort
import LiName.Types
import LiName.Undo
import LiName.Utils

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad.Reader (ask, runReaderT)
import Data.Either (lefts, rights)
import Data.Either.Combinators (mapLeft)
import Data.List ((\\), isPrefixOf)
import Data.Map (Map, fromList)
import Data.Map.Lazy (lookup)
import Prelude hiding (lookup, fail)
import System.Directory (getCurrentDirectory)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)



main' :: Either String (LiNameConfig, [String]) -> IO ()
main' (Left err)               = hPutStrLn stderr err
main' (Right (conf, pathArgs)) = flip runReaderT conf $ do
    pfs <- _pathFilters <$> ask
    lfs <- _lineFilters <$> ask
    lf <- _listFile <$> ask
    (common, ops) <- compactPath' (conf^.compact) <$> (loadListAndPath lf pathArgs >>= sortPathList)
    ps <- io $ filterCommands pfs ops
    let ss = makeSources ps
        oss = makeSources ops
        sm = fromList oss
        oss' = map sourceLine oss
    ss' <- io $ filterCommands lfs $ map sourceLine ss
    case conf^.pairList of
      Just prefix ->
        editAndPrintPairs prefix oss' ss' sm common >>= (retry sm common . lefts)
      _ ->
        editAndProcess oss' ss' sm common >>= (retry sm common . lefts)


retry :: Map LiNameKey LiNamePath -> String -> [LiNameFail] -> L ()
retry _ _ []          = return ()
retry sm common fails = do
    let ss = fails >>= \(x, y) -> ["# " ++ unbreak y, x]
    results <- editAndProcess ss ss sm common
    retry sm common $ lefts results


sourceLine :: LiNameSource -> String
sourceLine (LiNameKey key, fp) = printf "%.4d\t%s" key fp


editAndProcess
  :: [String]                 -- ^ Original lines
  -> [String]                 -- ^ Filterd lines
  -> Map LiNameKey LiNamePath -- ^ map: Key -> Original path
  -> String                   -- ^ Common path
  -> L [LiNameResult]         -- ^ Edited lines by text editor
editAndProcess oss ss sm common = do
    cwd <- io getCurrentDirectory
    ls <- (\\ oss) . filter (not . isPrefixOf "#") <$> edit ss
    results <- mapM (process sm common) ls
    io $ clean $ map snd $ rights results
    io $ saveUndoInfo cwd common $ rights results
    io $  putResult "input" (length ss) results
    return results


process :: Map LiNameKey LiNamePath -> LiNamePath -> String -> L LiNameResult
process sm common line =
    case readLine line of
      Left fail   -> return $ Left fail
      Right entry ->
          case findPath sm entry of
            Nothing    -> return $ Left (line, "Not found key: " ++ show (entry^.entryKey))
            Just fp  -> do
                r <- doAction common (entry^.action) fp
                case r of
                  Right ()  -> return $ Right (entry, fp)
                  Left err' -> return $ Left (line, err')


editAndPrintPairs
  :: Maybe String
  -> [String]                 -- ^ Original lines
  -> [String]                 -- ^ Filterd lines
  -> Map LiNameKey LiNamePath -- ^ map: Key -> Original path
  -> String                   -- ^ Common path
  -> L [LiNameResult]         -- ^ Edited lines by text editor
editAndPrintPairs prefix oss ss sm common = do
    cwd <- io getCurrentDirectory
    ls <- (\\ oss) . filter (not . isPrefixOf "#") <$> edit ss
    results <- mapM (printPairs prefix sm common) ls
    return results


printPairs :: Maybe String -> Map LiNameKey LiNamePath -> LiNamePath -> String -> L LiNameResult
printPairs prefix sm common line =
    case readLine line of
      Left fail   -> return $ Left fail
      Right entry ->
        case findPath sm entry of
          Nothing -> return $ Left (line, "Not found key: " ++ show (entry^.entryKey))
          Just fp -> do
            case entry^.action of
              DoRename t -> do
                  -- FIXME Correct filepath escaping
                  let prefix' = maybe "" (++ " ") prefix
                  io $ putStrLn $ printf "%s%s\t%s" prefix' (show fp) (show t)
                  return $ Right (entry, fp)
              _          -> return $ Left (line, "Cannot print copy action: " ++ show (entry^.entryKey))


readLine :: String -> Either LiNameFail LiNameEntry
readLine line = mapLeft ((line,) . show) $ parseEntry "" line


findPath :: Map LiNameKey LiNamePath -> LiNameEntry -> Maybe LiNamePath
findPath sm entry = lookup (entry^.entryKey) sm
