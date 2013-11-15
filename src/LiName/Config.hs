
{-# LANGUAGE ScopedTypeVariables #-}

module LiName.Config (
  loadConfigFile
) where


import LiName.Sort
import LiName.Types

import Prelude hiding (lookup)
import Control.Exception
import Control.Lens
import Data.Default (def)
import Data.Yaml.Config
import System.Directory (getHomeDirectory)
import System.FilePath (combine)
import Data.Text (pack)
import Text.JSON
import Text.JSON.Types
import Text.JSON.Generic
import Control.Applicative ((<$>))



loadConfigFile :: IO LiNameConfig
loadConfigFile = do
    home <- getHomeDirectory
    let fn = combine home ".liname.json"
    jsonFile <- readFile fn
    let root = decode jsonFile :: Result (JSObject JSValue)

    let fs = [ entry recursive toBool id True $ root >>= valFromObj "recursive"
             , entry compact toInt Just Nothing $ root >>= valFromObj "compact"
             ] :: [LiNameConfig -> LiNameConfig]

    return $ foldl (flip id) def fs


entry mut typef conv d json = set mut $ withDef typef conv d json


withDef :: (a -> JSValue -> a) -> (a -> b) -> b -> Result JSValue -> b
withDef f conv d v = case resultToEither v of
                       (Right v) -> conv $ f d v
                       (Left v)  -> d


toInt :: Int -> JSValue -> Int
toInt _ (JSRational _ v) = ceiling v
toInt d _                = d


toBool :: Bool -> JSValue -> Bool
toBool _ (JSRational _ 1) = True
toBool _ (JSRational _ _) = False
toBool d _                = d


makeCommand :: LiNameCommand -> Config -> LiNameCommand
makeCommand d c = LiNameCommand { _path = lookupDefault c (pack "command") (d^.path)
                                , _args = lookupDefault c (pack "options") (d^.args)
                                , _placeHolder = lookupDefault c (pack "place_holder") (d^.placeHolder) }


makeCommand' :: Config -> String -> LiNameCommand -> IO LiNameCommand
makeCommand' y n d = withDefault d $ makeCommand d <$> subconfig y (pack n)


withDefault :: a -> IO a -> IO a
withDefault d a = a `catch` catchAndReturnDefault d


catchAndReturnDefault :: a -> SomeException -> IO a
catchAndReturnDefault d _ = return d
