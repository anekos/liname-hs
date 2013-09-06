
{-# LANGUAGE ScopedTypeVariables, TupleSections, OverloadedStrings #-}

module LiName.Main (main', getConf) where


import LiName.Actions
import LiName.Clean
import LiName.Command
import LiName.Config
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
import Data.Either (lefts, rights)
import Data.Either.Unwrap (mapLeft)
import Data.Map (Map, fromList)
import Data.Map.Lazy (lookup)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (getFdStatus, isNamedPipe)
import System.Posix.IO (stdInput)
import Text.Printf (printf)



main' :: Either String (LiNameConfig, [String]) -> IO ()
main' (Left err)               = hPutStrLn stderr err
main' (Right (conf, pathArgs)) = do
    (common, ps) <- compactPath' (conf^.compact) <$> (sortPathList (conf^.sortType) =<< loadPath' pathArgs)
    let ss = makeSources ps
        sm = fromList ss
    results <- editAndProcess conf (map sourceLine ss) sm common
    retry conf sm common $ lefts results


retry :: LiNameConfig -> Map LiNameKey LiNamePath -> String -> [LiNameFail] -> IO ()
retry _ _ _ [] = return ()
retry conf sm common fails = do
    results <- editAndProcess conf (map fst fails) sm common
    retry conf sm common $ lefts results


getConf :: IO (Either String (LiNameConfig, [String]))
getConf = do
    fileConf <- loadConfigFile
    as <- getArgs
    fd <- getFdStatus stdInput
    pas <- if isNamedPipe fd then lines <$> getContents else return []
    parseOptions fileConf $ as ++ pas


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


editAndProcess :: LiNameConfig -> [String] -> Map LiNameKey LiNamePath -> String -> IO [LiNameResult]
editAndProcess conf ss sm common = do
    results <- edit (conf^.editorCommand) ss >>= mapM (process conf sm common)
    clean $ map snd $ rights results
    putResult (length ss) results
    return results


process :: LiNameConfig -> Map LiNameKey LiNamePath -> LiNamePath -> String -> IO LiNameResult
process conf sm common line =
    case readLine line of
      Left fail   -> return $ Left fail
      Right entry ->
          case findPath sm entry of
            Nothing    -> return $ Left (line, "Not found key: " ++ show (entry^.entryKey))
            Just fp  -> do
                r <- doAction conf common (entry^.action) fp
                case r of
                  Right ()  -> return $ Right (entry, fp)
                  Left err' -> return $ Left (line, err')


readLine :: String -> Either LiNameFail LiNameEntry
readLine line = mapLeft ((line,) . show) $ parseEntry "" line


findPath :: Map LiNameKey LiNamePath -> LiNameEntry -> Maybe LiNamePath
findPath sm entry = lookup (entry^.entryKey) sm


indent :: String -> String
indent = ("  " ++)
