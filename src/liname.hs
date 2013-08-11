
import           LiName.Types
import           LiName.Parsers

import           Control.Applicative
import           Control.Monad
import           Text.Parsec.Prim (runP)
import           Text.Parsec.Error (ParseError)



test :: String -> Either ParseError LinameEntry
test s = runP entryParser () "<TEST>" s

main = do ls <- lines <$> getContents
          forM_ ls (print . test)
