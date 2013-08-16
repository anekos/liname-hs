
import           LiName.Types
import           LiName.Parsers
import           LiName.Operators
import           LiName.Editor
import           LiName.Loader

import           Control.Applicative
import           Control.Monad
import           Data.Default
import           Text.Parsec.Prim (runP)
import           Text.Parsec.Error (ParseError)



test :: String -> Either ParseError LiNameEntry
test s = runP entryParser () "<TEST>" s

main = do ls <- lines <$> getContents
          forM_ ls (print . test)
