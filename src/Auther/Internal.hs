module Auther.Internal where

import Data.Binary as Bin
import Data.Binary.Get -- TODO use lazy equivalent? cereal?
import Data.ByteArray as BA
import Data.ByteString.Char8 as BS
import Data.ByteString as BS2
import System.Clock
import Crypto.Hash
import Crypto.MAC.HMAC
import Data.Array as A
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Either
import Data.Word8
import Data.Bits
import Data.Int
import System.Posix.Env.ByteString
import Data.ByteString.Conversion
import qualified Data.ByteString.Base32 as Base32 (decode)

generateCode :: TimeSpec -> ByteString -> Either String ByteString
generateCode timespec secret = do
  bsec <- Base32.decode secret
  let h = doIt timespec bsec
  let word = (bytesToWord32 . subDigest) h
  return $ (word32ToDecimal . mask31) word

--makeWord :: Bytes -> Word32
--makeWord s = runGet getWord32be (fromJust $ fromByteString s) :: Word32

bytesToWord32 :: Bytes -> Word32
bytesToWord32 bs = (byte 0 `shift` 24)
             .|. (byte 1 `shift` 16)
             .|. (byte 2 `shift` 8)
             .|. byte 3
        where byte n = fromIntegral $ BA.index bs n

word32ToDecimal :: Word32 -> ByteString
word32ToDecimal word32 = (padL _0 6) . BS.pack . show $ mm word32

mm :: Word32 -> Int
mm word32 = fromIntegral $ mod word32 1000000

padL :: Word8 -> Int -> ByteString -> ByteString
padL w n s
    | BS2.length s < n = BS2.concat [padding, s]
    | otherwise        = s
  where
    padding = (BS2.replicate (n - BS2.length s) w)

mask31 :: Word32 -> Word32
mask31 word = word .&. mask
  where
    mask = toEnum 2147483647 :: Word32

lastNibble :: Bytes -> Int
lastNibble bs = fromEnum $ end .&. mask
  where
    end = fromIntegral $ BA.index bs 39 -- change to length - 1 
    mask = toEnum 15 :: Word8

subDigest :: Bytes -> Bytes
subDigest dig = BA.take 4 $ BA.drop offset dig
  where
    offset = lastNibble dig

doIt :: TimeSpec -> ByteString -> Bytes
doIt timespec secret = BA.convert hm
  where 
    message = timeblock timespec
    hm = hmac secret message :: (HMAC SHA1)

timeblock :: TimeSpec -> ByteString
timeblock timespec = padL (toEnum 0) 8 $ toByteString' q
  where
    timestamp = toNanoSecs timespec
    q = quot timestamp 30000000000

-- 2fa apps vary on
-- * hash algorithm
-- * reset window
-- * code length
-- * secret length?
-- install Sophos Authenticator and copy options?
