module Lips.Eval
  ( eval
  ) where

import Lips.Parser

eval :: Result LipsVal -> IO ()
eval (Failure err) = print $ _errDoc err
eval (Success res) = print res
