module Parser where

import Control.Applicative
import Text.Trifecta

data LipsVal = LipsNumber Integer
             | LipsString String
             | LipsBool Bool
             | LipsAtom Atom
             deriving (Eq, Show)