{-
  stolen from qfpl/parsers-megaparsec
-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Megaparsec.Parsers
  ( ParsecT(..)
  , module Text.Parser.Char
  , module Text.Parser.Combinators
  , module Text.Parser.LookAhead
  , module Text.Parser.Token
  ) where

import           Control.Applicative
import           Control.Monad              (MonadPlus)
import           Control.Monad.Cont.Class   (MonadCont)
import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.Fail         (MonadFail)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState)
import           Control.Monad.Trans        (MonadTrans)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Text.Megaparsec            (MonadParsec, Stream, Token)
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MP
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token

newtype ParsecT e s m a = ParsecT { unParsecT :: MP.ParsecT e s m a }
  deriving
  ( Functor, Applicative, Alternative, Monad, MonadPlus, Semigroup
  , Monoid, MonadCont, MonadError e', MonadFail, MonadIO
  , MonadParsec e s, MonadReader r, MonadState st, MonadTrans
  )

instance (Ord e, Stream s) => Parsing (ParsecT e s m) where
  try = MP.try
  (<?>) = (MP.<?>)
  notFollowedBy = MP.notFollowedBy
  eof = MP.eof
  unexpected = MP.unexpected . MP.Label . NE.fromList

instance Ord e => CharParsing (ParsecT e String m) where
  satisfy = MP.satisfy
  char = MP.char
  notChar = MP.anySingleBut
  anyChar = MP.anySingle
  string = MP.string
  text t = t <$ string (T.unpack t)

instance Ord e => CharParsing (ParsecT e TL.Text m) where
  satisfy = MP.satisfy
  char = MP.char
  notChar = MP.anySingleBut
  anyChar = MP.anySingle
  string t = t <$ MP.string (TL.pack t)
  text t = t <$ MP.string (TL.fromStrict t)

instance Ord e => CharParsing (ParsecT e T.Text m) where
  satisfy = MP.satisfy
  char = MP.char
  notChar = MP.anySingleBut
  anyChar = MP.anySingle
  string t = t <$ MP.string (T.pack t)
  text = MP.string

instance (Ord e, Stream s) => LookAheadParsing (ParsecT e s m) where
  lookAhead = MP.lookAhead

instance Ord e => TokenParsing (ParsecT e String m)
instance Ord e => TokenParsing (ParsecT e T.Text m)
instance Ord e => TokenParsing (ParsecT e TL.Text m)
