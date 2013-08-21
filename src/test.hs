
import LiName.Actions
import LiName.Command
import LiName.Loader
import LiName.Operators
import LiName.Parsers
import LiName.Types

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Default
import Data.Either (lefts, rights)
import System.Environment (getArgs)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (runP)


main = do
    let conf = def :: LiNameConfig
    src <- getArgs
    es <- map entryLine <$> makeEntries <$> loadPath' src
    es' <- map (parseEntry "<TEMP>") <$> edit (def^.editorCommand) es
    print $ rights es'
    print $ lefts es'
