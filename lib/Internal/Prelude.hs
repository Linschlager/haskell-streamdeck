module Internal.Prelude
  ( module Control.Monad,
    module Control.Monad.Reader,
    module Data.Bits,
    module Data.ByteString,
    module Data.Functor,
    module Control.Lens.Combinators,
    module Control.Lens.Operators,
    module Data.Ord,
    module Data.Word,
    module GHC.Generics,
    module Prelude,
    module UnliftIO,
    module UnliftIO.IO,
    (<$$>)
  )
where

import Control.Lens.Combinators (view)
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Reader
import Data.Bits
import Data.ByteString (ByteString)
import Data.Functor
import Data.Generics.Labels ()
import Data.Ord
import Data.Word (Word16, Word8)
import GHC.Generics
import Prelude
import UnliftIO
import UnliftIO.IO

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap
