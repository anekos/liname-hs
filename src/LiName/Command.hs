
module LiName.Command (
  edit,
  run
) where

import LiName.Filter
import LiName.Types

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad.Reader (ask)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (waitForProcess, runProcess)



run :: LiNameCommand -> String -> IO ExitCode
run cmd arg = do
    let as = genArgs cmd arg
    runProcess (cmd^.path) as Nothing Nothing Nothing Nothing Nothing >>= waitForProcess


edit :: [String] -> L [String]
edit contents = do
    cmd <- _editorCommand <$> ask
    fs <- _filters <$> ask
    io $ do
      contents' <- lines <$> filterCommands fs (unlines contents)
      fn <- makeTempFile contents'
      run cmd fn
      ls <- lines <$> readFile fn
      removeFile fn
      return ls


makeTempFile :: [String] -> IO String
makeTempFile cs = do
    dir <- getTemporaryDirectory
    (fn, h) <- openTempFile dir "liname-.txt"
    hPutStr h $ unlines cs
    hClose h
    return fn


genArgs :: LiNameCommand -> String -> [String]
genArgs conf = replace (conf^.placeHolder) (conf^.args)


replace :: String -> [String] -> String -> [String]
replace f as t = map (\x -> if x == f then t else x) as
