
{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

module Main (main) where

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

import Prelude hiding (lookup, fail)
import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad (when)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Either (lefts, rights)
import Data.Either.Unwrap (mapLeft)
import Data.Map (Map, fromList)
import Data.Map.Lazy (lookup)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (getFdStatus, isNamedPipe)
import System.Posix.IO (stdInput)
import Text.Printf (printf)
import Text.ShellEscape (escape)



main :: IO ()
main = getConf >>= main'


main' :: Either String (LiNameConfig, [String]) -> IO ()
main' (Left err)               = hPutStrLn stderr err
main' (Right (conf, pathArgs)) = do
    (common, ps) <- compactPath' (conf^.compact) <$> (sortPathList (conf^.sortType) =<< loadPath' pathArgs)
    let ss = makeSources ps
    results <- edit (conf^.editorCommand) (map sourceLine ss) >>= mapM (process conf (fromList ss) common)
    clean $ map snd $ rights results
    putResult (length ss) results


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
    when (null fails) $ do
      hPutStrLn stderr "Fails"
      mapM_ (hPutStrLn stderr . indent . snd) fails


sourceLine :: LiNameSource -> String
sourceLine (LiNameKey key, fp) = printf "%.4d\t%s" key $ toString $ escape $ fromString fp


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
                  Left err' -> return $ Left (fp, err')


readLine :: String -> Either LiNameFail LiNameEntry
readLine line = mapLeft ((line,) . show) $ parseEntry "" line


findPath :: Map LiNameKey LiNamePath -> LiNameEntry -> Maybe LiNamePath
findPath sm entry = lookup (entry^.entryKey) sm


indent :: String -> String
indent = ("  " ++)
