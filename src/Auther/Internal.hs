module Auther.Internal where

import Data.Binary as Bin
import Data.Binary.Get -- TODO use lazy equivalent? cereal?
import Data.ByteArray
import Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.ByteString as BS2
import System.Clock
import Crypto.Hash
import Crypto.Hash.Algorithms
import Crypto.MAC.HMAC
import Data.Array as A
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Word8
import Data.Bits
import System.Posix.Env.ByteString
import Data.ByteString.Conversion

generateCode :: TimeSpec -> ByteString -> ByteString
generateCode timespec secret = (word32ToDecimal . mask31) word
  where
    h = doIt timespec secret
    s = subDigest h
    word = makeWord s

makeWord :: ByteString -> Word32
makeWord s = runGet getWord32be (fromJust $ fromByteString s) :: Word32

word32ToDecimal :: Word32 -> ByteString
word32ToDecimal word32 = (padL _0 6) . BS.pack . show $ mm word32

mm :: Word32 -> Int
mm word32 = fromIntegral $ mod word32 1000000

padL :: Word8 -> Int -> ByteString -> ByteString
padL w n s
    | BS2.length s < n  = BS2.concat [padding, s]
    | otherwise        = s
  where
    padding = (BS2.replicate (n - BS2.length s) w)

mask31 :: Word32 -> Word32
mask31 word = word .&. mask
  where
    mask = toEnum 2147483647 :: Word32

extractDecoder :: Decoder Word32 -> Word32
extractDecoder (Done _ _ a) = a  -- TODO handle failure cases

lastNibble :: ByteString -> Int
lastNibble bs = fromEnum $ end .&. mask
  where
    end = fromIntegral $ (BS2.last bs)
    mask = toEnum 15 :: Word8

subDigest :: ByteString -> ByteString
subDigest dig = BS.take 4 $ BS.drop offset dig
  where
    offset = lastNibble dig

doIt :: TimeSpec -> ByteString -> BS.ByteString
doIt timespec secret = BS.pack $ hm
  where 
    t = timeblock timespec
    s = xxx $ decode (LBS.fromStrict secret)
    hm = show $ hmacGetDigest $ (hmac s t :: HMAC SHA1)

xxx :: Either String ByteString -> ByteString
--xxx (Left a) = BS.pack "error"
xxx (Right a) = a

timeblock :: TimeSpec -> ByteString
timeblock timespec = padL (toEnum 0) 8 $ LBS.toStrict $ Bin.encode q
  where
    timestamp = toNanoSecs timespec
    q = quot timestamp 30000000000
