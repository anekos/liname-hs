
{-# LANGUAGE TupleSections #-}

module LiName.Path (
  compactPath'
) where

import System.FilePath.Posix (joinPath, splitDirectories)



commonPath :: [String] -> [String] -> [String]
commonPath [_] [_] = []
commonPath (x:xs) (y:ys)
    | x == y      = x : commonPath xs ys
    | otherwise   = []
commonPath _ _    = []


commonPath' :: [[String]] -> [String]
commonPath' []         = []
commonPath' [x]        = x
commonPath' (x1:x2:xs) = commonPath' $ commonPath x1 x2 : xs


compactPath :: Int -> [String] -> (String, [String])
compactPath _ [x] = let p = splitDirectories x
                        cut = max 0 $ length p - 1
                    in  (joinPath $ take cut p, [joinPath $ drop cut p])
compactPath lv xs = let ps = map splitDirectories xs
                        shortest = minimum $ map length ps
                        cp = commonPath' ps
                        cpl = length cp
                        cut = min (shortest - 1) $ max 0 $ cpl  - lv
                    in  (joinPath $ take cut cp, map (joinPath . drop cut) ps)


compactPath' :: Maybe Int -> [String] -> (String, [String])
compactPath' = maybe (("",) . id) compactPath
