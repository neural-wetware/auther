module Base32Decode (base32decode) where

import Data.ByteString
import Data.Word8
import Data.Maybe

convert32 :: Word8 -> Maybe Word8
convert32 a
    | isLower(a) = Just $ fromIntegral $ a - 97
    | isUpper(a) = Just $ fromIntegral $ a - 65
    | isDigit(a) = Just $ fromIntegral $ a - 24 -- TODO only allow digits 2-7
    | otherwise = Nothing

base32decode :: ByteString -> Maybe ByteString
base32decode bs = fmap pack $ sequence (Prelude.map convert32 $ unpack bs)
