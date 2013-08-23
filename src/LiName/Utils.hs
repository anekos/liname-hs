
module LiName.Utils where


notDots :: String -> Bool
notDots "."  = False
notDots ".." = False
notDots _    = True

