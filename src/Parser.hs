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

lipsString :: Parser LipsVal
lipsString = do
  char '"'
  str <- many (noneOf "\"")
  char '"'
  pure (LipsString str)

lipsNumber :: Parser LipsVal
lipsNumber = LipsNumber <$> integer

parseLips :: Parser LipsVal
parseLips =  lipsAtom
         <|> lipsString
         <|> lipsNumber

readExpr :: String -> Result LipsVal
readExpr input = parseString parseLips mempty input
