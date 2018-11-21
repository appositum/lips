module Lips.Parser
  ( parse
  , readExpr
  , LipsVal(..)
  , ParseError
  ) where

import           Control.Applicative
import           Data.Char               (digitToInt)
import           Data.Functor.Identity   (Identity)
import           Data.List               (foldl')
import           Data.Void               (Void)
import           Text.Megaparsec         (ParseErrorBundle, runParser)
import           Text.Megaparsec.Parsers

type Parser a = ParsecT Void String Identity a
type ParseError = ParseErrorBundle String Void

parse :: Parser a -> String -> String -> Either ParseError a
parse = runParser . unParsecT


data LipsVal = LipsInteger Integer
             | LipsFloat Double
             | LipsString String
             | LipsBool Bool
             | LipsAtom String
             | LipsList [LipsVal]
             | LipsDottedList [LipsVal] LipsVal
             deriving Eq

instance Show LipsVal where
  show (LipsString str) = "\"" ++ str ++ "\""
  show (LipsAtom name)  = name
  show (LipsInteger n)  = show n
  show (LipsFloat n)    = show n
  show (LipsBool True)  = "#t"
  show (LipsBool False) = "#f"
  show (LipsList list)  = "'(" ++ unwords (map show list) ++ ")"

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
lipsList = LipsList <$> parseLips `sepBy` spaces

lipsDottedList :: Parser LipsVal
lipsDottedList = do
  head <- parseLips `endBy` spaces
  tail <- char '.' >> spaces >> parseLips
  pure $ LipsDottedList head tail

lipsQuoted :: Parser LipsVal
lipsQuoted = do
  char '\''
  x <- parseLips
  pure $ LipsList [LipsAtom "quote", x]

toDecimal :: Integer -> String -> Integer
toDecimal base =
  foldl' (\x z -> base * x + fromIntegral (digitToInt z)) 0

lipsBin :: Parser LipsVal
lipsBin = fmap LipsInteger $ do
  char '0'
  oneOf "bB"
  b <- some (oneOf "01")
  pure $ toDecimal 2 b

lipsOct :: Parser LipsVal
lipsOct = fmap LipsInteger $ do
  char '0'
  oneOf "oO"
  o <- some (oneOf ['0'..'7'])
  pure $ toDecimal 8 o

lipsHex :: Parser LipsVal
lipsHex = fmap LipsInteger $ do
  char '0'
  oneOf "xX"
  h <- some $ digit <|> oneOf (['a'..'f'] ++ ['A'..'F'])
  pure $ toDecimal 16 h

parseLips :: Parser LipsVal
parseLips =  lipsString
         <|> try lipsFloat
         <|> try lipsBin
         <|> try lipsHex
         <|> try lipsOct
         <|> try lipsInteger
         <|> lipsAtom
         <|> lipsQuoted
         <|> parens (try lipsList <|> lipsDottedList)

readExpr :: String -> Either ParseError LipsVal
readExpr = parse parseLips "(input)"
