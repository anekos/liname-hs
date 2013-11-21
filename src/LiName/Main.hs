
{-# LANGUAGE ScopedTypeVariables, TupleSections, OverloadedStrings #-}

module LiName.Main (main', getConf) where


import LiName.Actions
import LiName.Clean
import LiName.Command
import LiName.Config
import LiName.Filter
import LiName.Loader
import LiName.Options
import LiName.Parsers
import LiName.Path
import LiName.Sort
import LiName.Types
import LiName.Utils

import Prelude hiding (lookup, fail)
import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad (forM_, unless)
import Control.Monad.Reader (ask, runReaderT)
import Data.Either (lefts, rights)
import Data.Either.Unwrap (mapLeft)
import Data.List ((\\), isPrefixOf)
import Data.Map (Map, fromList)
import Data.Map.Lazy (lookup)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (getFdStatus, isNamedPipe)
import System.Posix.IO (stdInput)
import Text.Printf (printf)



main' :: Either String (LiNameConfig, [String]) -> IO ()
main' (Left err)               = hPutStrLn stderr err
main' (Right (conf, pathArgs)) = flip runReaderT conf $ do
    pfs <- _pathFilters <$> ask
    lfs <- _lineFilters <$> ask
    (common, ops) <- compactPath' (conf^.compact) <$> (loadPath pathArgs >>= sortPathList)
    ps <- io $ filterCommands pfs ops
    let ss = makeSources ps
        oss = makeSources ops
        sm = fromList oss
        oss' = map sourceLine oss
    ss' <- io $ filterCommands lfs $ map sourceLine ss
    results <- editAndProcess oss' ss' sm common
    retry sm common $ lefts results


retry :: Map LiNameKey LiNamePath -> String -> [LiNameFail] -> L ()
retry _ _ []          = return ()
retry sm common fails = do
    let ss = fails >>= \(x, y) -> ["# " ++ unbreak y, x]
    results <- editAndProcess ss ss sm common
    retry sm common $ lefts results


getConf :: IO (Either String (LiNameConfig, [String]))
getConf = do
    fileConf <- loadConfigFile
    as <- getArgs
    fd <- getFdStatus stdInput
    pas <- if isNamedPipe fd then lines <$> getContents else return []
    parseOptions False fileConf $ as ++ pas


putResult :: Int -> [LiNameResult] -> IO ()
putResult inputs rs = do
    let fails = lefts rs
    putStrLn "Results"
    putStrLn $ printf (indent "input:    %4d") inputs
    putStrLn $ printf (indent "try:      %4d") $ length rs
    putStrLn $ printf (indent "success:  %4d") $ length $ rights rs
    putStrLn $ printf (indent "fail:     %4d") $ length fails
    unless (null fails) $ do
      hPutStrLn stderr "Fails"
      forM_ fails $ \(line, err) -> hPutStrLn stderr $ indent line ++ "\n" ++ indent ('\t' : unbreak err)


sourceLine :: LiNameSource -> String
sourceLine (LiNameKey key, fp) = printf "%.4d\t%s" key fp


editAndProcess
  :: [String]                 -- ^ Original lines
  -> [String]                 -- ^ Filterd lines
  -> Map LiNameKey LiNamePath -- ^ map: Key -> Original path
  -> String                   -- ^ Common path
  -> L [LiNameResult]         -- ^ Edited lines by text editor
editAndProcess oss ss sm common = do
    ls <- (\\ oss) . filter (not . isPrefixOf "#") <$> edit ss
    results <- mapM (process sm common) ls
    io $ clean $ map snd $ rights results
    io $  putResult (length ss) results
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


readLine :: String -> Either LiNameFail LiNameEntry
readLine line = mapLeft ((line,) . show) $ parseEntry "" line


findPath :: Map LiNameKey LiNamePath -> LiNameEntry -> Maybe LiNamePath
findPath sm entry = lookup (entry^.entryKey) sm


indent :: String -> String
indent = ("  " ++)
