
module LiName.Parsers where

import           LiName.Types
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error


actionParser :: Parser (LinamePath -> LinameAction)
actionParser = do name <- many $ noneOf "0123456789"
                  case name of
                    "!!" -> return $ const DoDelete
                    "!" -> return $ const DoTrash
                    "" -> return $ DoRename
                    _ -> unexpected name


keyParser :: Parser LinameKey
keyParser = many1 digit >>= return . LinameKey . read


entryParser :: Parser LinameEntry
entryParser = do act <- actionParser
                 key <- keyParser
                 char '\t'
                 path <- many1 anyChar
                 return $ LinameEntry key path (act path)
