module Main where

import Control.Monad (unless)
import Parser
import System.Environment
import System.IO

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    [] -> repl
    (path:_) -> readFile path >>= eval . readExpr

replRead :: IO String
replRead = do
  putStr "LIPS> "
  hFlush stdout
  getLine

repl :: IO ()
repl = do
  input <- replRead
  unless (input == ":q" || input == ":quit") $ do
    eval $ readExpr input
    main
