module Base32Decode (base32decode) where

import Data.ByteString
import Data.Word8
import Data.Maybe
import Base32Decode.Internal

base32decode :: ByteString -> Maybe ByteString
base32decode bs = fmap pack $ sequence (Prelude.map convert32 $ unpack bs)
