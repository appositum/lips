module Lips.Parser
  ( parse
  , readExpr
  , Error
  , LipsVal(..)
  , Result(..)
  ) where

import Control.Applicative
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Text.Megaparsec (runParser, ParseErrorBundle)
import Text.Megaparsec.Parsers

type Parser a = ParsecT Void String Identity a
type Error = ParseErrorBundle String Void
type Result = Either Error

parse :: Parser a -> String -> String -> Result a
parse = runParser . unParsecT


data LipsVal = LipsInteger Integer
             | LipsFloat Double
             | LipsString String
             | LipsBool Bool
             | LipsAtom String
             | LipsList [LipsVal]
             deriving Eq

instance Show LipsVal where
  show (LipsString str) = "\"" ++ str ++ "\""
  show (LipsAtom name) = name
  show (LipsInteger n) = show n
  show (LipsFloat n) = show n
  show (LipsBool True) = "#t"
  show (LipsBool False) = "#f"
  show (LipsList list) = "'(" ++ unwords (map show list) ++ ")"

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
  pure $ LipsString (concat str)

lipsInteger :: Parser LipsVal
lipsInteger = LipsInteger <$> integer

lipsFloat :: Parser LipsVal
lipsFloat = LipsFloat <$> (signed <*> double) where
  signed =  negate <$ char '-'
        <|> id <$ char '+'
        <|> pure id

lipsList :: Parser LipsVal
lipsList = do
  symbol "'("
  lst <- LipsList <$> sepBy parseLips spaces
  symbolic ')'
  pure lst

parseLips :: Parser LipsVal
parseLips =  lipsString
         <|> try lipsFloat
         <|> lipsInteger
         <|> lipsAtom
         <|> lipsList

readExpr :: String -> Result LipsVal
readExpr = parse parseLips "Syntax error"
