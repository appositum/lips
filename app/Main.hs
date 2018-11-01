module Main where

import Data.Char
import Parser
import System.Environment
import Text.Trifecta

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    [] -> putStrLn "lips: no input file"
    (path:_) -> readFile path >>= print . readExpr
