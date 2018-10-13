module Main where

import Parser
import System.Environment

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    [] -> putStrLn "lips: no input file"
    (path:_) -> readFile path >>= print . readExpr
