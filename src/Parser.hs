module Parser where

import Control.Applicative
import Text.Trifecta

data LipsVal = LipsInteger Integer
             | LipsFloat Double
             | LipsString String
             | LipsBool Bool
             | LipsAtom String
             | LipsList [LipsVal]
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

lipsInteger :: Parser LipsVal
lipsInteger = LipsInteger <$> integer

lipsFloat :: Parser LipsVal
lipsFloat = fmap LipsFloat $ signed <*> double where
  signed =  negate <$ char '-'
        <|> id <$ char '+'
        <|> pure id

lipsList :: Parser LipsVal
lipsList = do
  char '\''
  char '('
  spaces
  lst <- LipsList <$> sepBy parseLips spaces
  char ')'
  pure lst

parseLips :: Parser LipsVal
parseLips =  lipsString
         <|> try lipsFloat
         <|> lipsInteger
         <|> lipsAtom
         <|> lipsList

readExpr :: String -> Result LipsVal
readExpr = parseString parseLips mempty
