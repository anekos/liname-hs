
import LiName.Types
import LiName.Parsers
import LiName.Operators
import LiName.Editor
import LiName.Loader

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mapM)
import Data.Default (def)
import Data.Either (rights, lefts)
import System.Environment (getArgs)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (runP)



main = do
    src <- getArgs
    es <- map entryLine <$> makeEntries <$> loadPath' src
    es' <- map (parseEntry "<TEMP>") <$> edit def es
    print $ rights es'
    print $ lefts es'
