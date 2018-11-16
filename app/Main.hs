module Main where

import Control.Monad (unless)
import Lips
import System.Environment
import System.IO

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    [] -> repl
    (x:_) -> replEval (readExpr x)

replEval (Left  err) = printError err
replEval (Right res) = print $ eval res

repl :: IO ()
repl = do
  input <- putStr "LIPS> " *> hFlush stdout *> getLine
  unless (input == ":q" || input == ":quit") $ do
    replEval (readExpr input)
    repl
