module Lips.Eval
  ( eval
  , printError
  ) where

import Lips.Parser
import Text.Megaparsec (errorBundlePretty)

printError :: Error -> IO ()
printError = putStr . errorBundlePretty

eval :: LipsVal -> LipsVal
eval val@(LipsString _) = val
eval val@(LipsInteger _) = val
eval val@(LipsAtom _) = val
