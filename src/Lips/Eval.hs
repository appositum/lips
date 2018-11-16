module Lips.Eval
  ( eval
  , printError
  ) where

import Data.List (foldl1')
import Lips.Parser
import Text.Megaparsec (errorBundlePretty)

printError :: Error -> IO ()
printError = putStr . errorBundlePretty

unpack :: LipsVal -> Integer
unpack (LipsList [n]) = unpack n
unpack (LipsInteger n) = n
unpack (LipsString str) =
  let parsed = reads str :: [(Integer, String)]
  in if null parsed then 0 else fst (parsed !! 0)
unpack _ = 0

numericOp :: (Integer -> Integer -> Integer) -> [LipsVal] -> LipsVal
numericOp op params = LipsInteger $ foldl1' op (map unpack params)

operations :: [(String, [LipsVal] -> LipsVal)]
operations =
  [ ("+", numericOp (+))
  , ("-", numericOp (-))
  , ("*", numericOp (*))
  , ("/", numericOp div)
  , ("mod", numericOp mod)
  , ("rem", numericOp rem)
  , ("quot", numericOp quot)
  ]

apply :: String -> [LipsVal] -> LipsVal
apply func args = maybe (LipsBool False) ($ args) $ lookup func operations

eval :: LipsVal -> LipsVal
eval val@(LipsString _) = val
eval val@(LipsInteger _) = val
eval val@(LipsAtom _) = val
eval (LipsList (LipsAtom func:args)) = apply func (map eval args)
