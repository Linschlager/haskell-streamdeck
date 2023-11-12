module Data.ByteString.Extra where

import Data.ByteString qualified as BS
import Data.List.Extra qualified as List
import Internal.Prelude

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf i = fmap BS.pack . List.chunksOf i . BS.unpack
