
module LiName.Path (
  compactPath'
) where

import System.FilePath.Posix (joinPath, splitDirectories)



commonPath :: [String] -> [String] -> [String]
commonPath (x:xs) (y:ys)
    | x == y      = x : commonPath xs ys
    | otherwise   = []
commonPath _ _    = []


commonPath' :: [[String]] -> [String]
commonPath' []         = []
commonPath' [x]        = x
commonPath' (x1:x2:xs) = commonPath' $ commonPath x1 x2 : xs


commonPathLevel :: [[String]] -> Int
commonPathLevel = length . commonPath'


compactPath :: Int -> [String] -> [String]
compactPath lv xs = map (joinPath . compact) ps
  where
    ps = map splitDirectories xs
    compact = drop $ max 0 $ commonPathLevel ps - lv


compactPath' :: Maybe Int -> [String] -> [String]
compactPath' = maybe id compactPath
