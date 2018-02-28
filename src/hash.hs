module Main where

--The service provider generates an 80-bit secret key for each user (whereas RFC 4226 §4 requires 128 bits and recommends 160 bits).[39] This is provided as a 16, 26 or 32 character base32 string or as a QR code. The client creates an HMAC-SHA1 using this secret key. The message that is HMAC-ed can be:
--
--the number of 30-second periods having elapsed since the Unix epoch (TOTP); or
--the counter that is incremented with each new code (HOTP).
--A portion of the HMAC is extracted and converted to a six-digit code.
--
--Pseudocode for one-time password (OTP)[edit]
--  function GoogleAuthenticatorCode(string secret)
--      key := base32decode(secret)
--      message := floor(current Unix time / 30)
--      hash := HMAC-SHA1(key, message)
--      offset := last nibble of hash
--      truncatedHash := hash[offset..offset+3]  //4 bytes starting at the offset
--      Set the first bit of truncatedHash to zero  //remove the most significant bit
--      code := truncatedHash mod 1000000
--      pad code with 0 until length of code is 6
--      return code

--  public function getCode($secret, $timeSlice = null)
--  {
--      if ($timeSlice === null) {
--          $timeSlice = floor(time() / 30);
--      }
--      $secretkey = $this->_base32Decode($secret);
--      // Pack time into binary string
--      $time = chr(0).chr(0).chr(0).chr(0).pack('N*', $timeSlice);
--      // Hash it with users secret key
--      $hm = hash_hmac('SHA1', $time, $secretkey, true);
--      // Use last nipple of result as index/offset
--      $offset = ord(substr($hm, -1)) & 0x0F;
--      // grab 4 bytes of the result
--      $hashpart = substr($hm, $offset, 4);
--      // Unpak binary value
--      $value = unpack('N', $hashpart);
--      $value = $value[1];
--      // Only 32 bits
--      $value = $value & 0x7FFFFFFF;
--      $modulo = pow(10, $this->_codeLength);
--      return str_pad($value % $modulo, $this->_codeLength, '0', STR_PAD_LEFT);
--  }

--{-# LANGUAGE OverloadedStrings #-}

import Data.Binary as Bin
import Data.ByteArray
import Data.ByteString.Char8 as BS
import System.Clock
import Crypto.Hash.Algorithms
import Crypto.MAC.HMAC
import Data.Array

--foo :: ByteArrayAccess a => a -> Int
--foo s = Data.ByteArray.length s
--
--callFoo :: Int
--callFoo = foo "asdf"

callHMAC :: ByteString -> HMAC SHA1
callHMAC secret = hmac secret (BS.pack "fff")

main :: IO ()
main = do
  timespec <- getTime Realtime
  print $ hmacGetDigest $ callHMAC (authenticate (BS.pack "secret"))
  --print $ hmacGetDigest $ xxx "asdfasfd" timespec
  --print $ digestToByteString $ sha1 $ pack "asdf"

--xxx :: String -> TimeSpec -> HMAC SHA1
--xxx secret timespec = hmac secret (timeblock timespec)

--timeblock :: TimeSpec -> ByteString
--timeblock timespec = Bin.encode q
--  where
--    timestamp = toNanoSecs timespec
--    q = quot timestamp 30000


table :: Array Integer Word8
table = array [
    (0, A),
    (1, B),
    (2, C),
    (3, D),
    (4, E),
    (5, F),
    (6, G),
    (7, H),
    (8, I),
    (9, J),
    (10, K),
    (11, L),
    (12, M),
    (13, N),
    (14, O),
    (15, P),
    (16, Q),
    (17, R),
    (18, S),
    (19, T),
    (20, U),
    (21, V),
    (22, W),
    (23, X),
    (24, Y),
    (25, Z),
    (26, 2),
    (27, 3),
    (28, 4),
    (29, 5),
    (30, 6),
    (31, 7)]

base32decode :: String -> Maybe ByteString
base32decode x = 
          
authenticate :: ByteString -> ByteString
authenticate secret = xxx (base32decode secret)
  where
    xxx (Left a) = (BS.pack a) -- need to handle error properly
    xxx (Right b) = b
--
--sha1 :: ByteString -> Digest SHA1
--sha1 = hash

--  function GoogleAuthenticatorCode(string secret)
--      key := base32decode(secret)
--      message := floor(current Unix time / 30)
--      hash := HMAC-SHA1(key, message)
--      offset := last nibble of hash
--      truncatedHash := hash[offset..offset+3]  //4 bytes starting at the offset
--      Set the first bit of truncatedHash to zero  //remove the most significant bit
--      code := truncatedHash mod 1000000
--      pad code with 0 until length of code is 6
--      return code

