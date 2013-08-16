
module LiName.Parsers (
  parseEntry
) where

import LiName.Types

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Prim (runP)



parseEntry :: String -> String -> Either ParseError LiNameEntry
parseEntry = runP entryParser ()


actionParser :: Parser (LiNamePath -> LiNameAction)
actionParser = do name <- many $ noneOf "0123456789"
                  case name of
                    "!!" -> return $ const DoDelete
                    "!" -> return $ const DoTrash
                    "" -> return $ DoRename
                    _ -> unexpected name


keyParser :: Parser LiNameKey
keyParser = many1 digit >>= return . LiNameKey . read


entryParser :: Parser LiNameEntry
entryParser = do act <- actionParser
                 key <- keyParser
                 char '\t'
                 path <- many1 anyChar
                 return $ LiNameEntry key path (act path)
