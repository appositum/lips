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
    (path:_) -> readFile path >>= eval . readExpr

repl :: IO ()
repl = do
  input <- putStr "LIPS> " *> hFlush stdout *> getLine
  unless (input == ":q" || input == ":quit") $ do
    eval $ readExpr input
    repl
