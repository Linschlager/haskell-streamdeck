module Data.ByteString.Extra where
    
import Data.List.Extra qualified as List
import Data.ByteString ( pack, unpack, ByteString )

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf i = fmap pack . List.chunksOf i . unpack