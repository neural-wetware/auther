module Base32Decode.Internal (convert32) where

import Data.ByteString
import Data.Word8
import Data.Maybe

convert32 :: Word8 -> Maybe Word8
convert32 a
    | isLower(a) = Just $ fromIntegral $ a - 97
    | isUpper(a) = Just $ fromIntegral $ a - 65
    | isDigit(a) = Just $ fromIntegral $ a - 24 -- TODO only allow digits 2-7
    | otherwise = Nothing
