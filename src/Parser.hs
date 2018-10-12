module Parser where

import Control.Applicative
import Text.Trifecta

data LipsVal = LipsNumber Integer
             | LipsString String
             | LipsBool Bool
             | LipsAtom String
             deriving (Eq, Show)

lipsSymbol :: Parser Char
lipsSymbol = oneOf "!#$%&|*+-/:><?=@^_~"

lipsAtom :: Parser LipsVal
lipsAtom = do
  first <- letter <|> lipsSymbol
  rest  <- many (letter <|> digit <|> lipsSymbol)
  let atom = first : rest
  pure $
    case atom of
      "#t" -> LipsBool True
      "#f" -> LipsBool False
      _    -> LipsAtom atom

escape :: Parser String
escape = do
  q <- char '\\'
  c <- oneOf "\\\"0nrvtbf"
  pure [q, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

lipsString :: Parser LipsVal
lipsString = do
  char '"'
  str <- many $ pure <$> nonEscape <|> escape
  char '"'
  pure (LipsString $ concat str)

lipsNumber :: Parser LipsVal
lipsNumber = LipsNumber <$> integer

parseLips :: Parser LipsVal
parseLips =  lipsAtom
         <|> lipsString
         <|> lipsNumber

readExpr :: String -> Result LipsVal
readExpr = parseString parseLips mempty
