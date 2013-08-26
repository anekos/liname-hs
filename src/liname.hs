
{-# LANGUAGE ScopedTypeVariables #-}

import LiName.Actions
import LiName.Clean
import LiName.Command
import LiName.Config
import LiName.Loader
import LiName.Options
import LiName.Parsers
import LiName.Types

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad (forM, when)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Either (lefts, rights)
import Data.Map (Map, fromList)
import Data.Map.Lazy (lookup)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.ShellEscape (escape)



main :: IO ()
main = do
    (conf, pathArgs) <- getConf
    ss :: [LiNameSource] <- makeSources <$> loadPath' pathArgs
    es' <- map (parseEntry "<TEMP>") <$> edit (def^.editorCommand) (map sourceLine ss)
    results <- forM (rights es') $ process conf (fromList ss)
    clean $ rights results
    putResult ss (rights es') results


getConf :: IO (LiNameConfig, [String])
getConf = do
    fileConf <- loadConfigFile
    getArgs >>= parseOptions fileConf


putResult :: [LiNameSource] -> [LiNameEntry] -> [Either String LiNamePath] -> IO ()
putResult ss es rs = do
    let sl = length ss
        el = length es
        rrl = length $ rights rs
        rll = length $ lefts rs
    putStrLn "Results"
    putStrLn $ printf (indent "input:    %4d") sl
    putStrLn $ printf (indent "try:      %4d") el
    putStrLn $ printf (indent "success:  %4d") rrl
    putStrLn $ printf (indent "fail:     %4d") rll
    when (rll > 0) $ do
      hPutStrLn stderr "Fails"
      mapM_ (hPutStrLn stderr . indent) $ lefts rs


sourceLine :: LiNameSource -> String
sourceLine (LiNameKey key, fp) = printf "%.4d\t%s" key $ toString $ escape $ fromString fp


process :: LiNameConfig -> Map LiNameKey LiNamePath -> LiNameEntry -> IO (Either String LiNamePath)
process conf sm (LiNameEntry {_entryKey = k, _action = a })
    | Just fp <- lookup k sm = (>> Right fp) <$> doAction conf a fp
    | otherwise              = return $ Left $ "Not found key: " ++ show k


indent :: String -> String
indent = ("  " ++)
