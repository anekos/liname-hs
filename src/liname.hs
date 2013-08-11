
import           LiName.Types
import           LiName.Parsers

import           Text.Parsec.Prim (runP)
import           Text.Parsec.Error (ParseError)



test :: String -> Either ParseError LinameEntry
test s = runP entryParser () "<TEST>" s

