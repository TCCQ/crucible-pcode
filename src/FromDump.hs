
module FromDump (parseDump) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text

import Analysis (PFunction(..), PBlock(..), mkPFunction)
import PCode (PInst)


bodyLine = do
  contents <- manyTill (anyChar) endOfLine
  return $ read @PInst contents

headLine = do
  string "FUNCTION"
  char '\t'
  name <- manyTill (alphaNum <|> (char '_')) endOfLine
  return name

singleF = do
  name <- headLine
  body <- manyTill bodyLine $ try $ lookAhead ((headLine >> return ()) <|> eof)
  return (name, body)

parseDump :: FilePath -> IO (Either String [PFunction])
parseDump path = do
  val <- parseFromFile (many singleF) path
  return $ case val of
    Right zipList -> Right $ map (\(name, instList) ->
                                    mkPFunction name instList) zipList
    Left pError -> Left $ show pError





