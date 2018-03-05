module Base32Decode (base32decode) where

import Data.ByteString
import Data.Array
import Data.Word8
import Data.Maybe

table :: Array Word8 Int 
table = array (_A, _Z) [
    (_A, 0),
    (_B, 1),
    (_C, 2),
    (_D, 3),
    (_E, 4),
    (_F, 5),
    (_G, 6),
    (_H, 7),
    (_I, 8),
    (_J, 9),
    (_K, 10),
    (_L, 11),
    (_M, 12),
    (_N, 13),
    (_O, 14),
    (_P, 15),
    (_Q, 16),
    (_R, 17),
    (_S, 18),
    (_T, 19),
    (_U, 20),
    (_V, 21),
    (_W, 22),
    (_X, 23),
    (_Y, 24),
    (_Z, 25)]

convert32 :: Word8 -> Maybe Word8
convert32 a
    | isUpper(a) = Just $ fromIntegral $ table ! a
    | isDigit(a) = Just $ fromIntegral $ a + 24
    | otherwise = Nothing

base32decode :: ByteString -> Maybe ByteString
base32decode bs = fmap pack $ sequence (Prelude.map convert32 $ unpack bs)
