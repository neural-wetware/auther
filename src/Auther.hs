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

import System.Clock
import Control.Monad
import Data.Either
import System.Posix.Env.ByteString
import Data.ByteString.Char8 as BS
import Auther.Internal

main :: IO ()
main = do
  timespec <- getTime Realtime
  (secret:_) <- getArgs
  BS.putStr $ either pack id (generateCode timespec secret)
