module Internal.Prelude
    ( module Control.Monad
    , module Control.Monad.Base
    , module Control.Monad.Fix
    , module Control.Monad.Reader
    , module Data.Bits
    , module Data.ByteString
    , module Data.Maybe
    , module Data.Functor
    , module Control.Lens.Combinators
    , module Control.Lens.Operators
    , module Data.Ord
    , module Data.Word
    , module GHC.Generics
    , module Prelude
    , module UnliftIO
    , module UnliftIO.IO
    , (<$$>)
    )
where

import Control.Lens.Combinators (view)
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Bits
import Data.ByteString (ByteString)
import Data.Functor
import Data.Generics.Labels ()
import Data.Maybe
import Data.Ord
import Data.Word (Word16, Word8)
import GHC.Generics
import UnliftIO
import UnliftIO.IO
import Prelude

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap
