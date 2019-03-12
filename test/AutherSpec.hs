module Main (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Auther.Internal
import System.Clock
import Data.ByteString.Char8
import qualified Data.ByteString as BSW
import Data.Word8
import Data.Word
import Data.Char (ord)
import Data.Maybe
import qualified Data.ByteArray as BA
import Data.Binary.Get -- TODO use lazy equivalent? cereal?
import Data.Binary (encode)
import Data.ByteString.Conversion
import qualified Data.ByteString.Base32 as B32 (decode)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "subDigest" $ do
        it "gives us a 4 byte substring based on last nibble offset" $ do
            (subDigest $ pack "hello there beautiful worldN") `shouldBe` (pack "auti")

    describe "lastNibble" $ do
        it "gets last nibble of bytestring" $ do
            (lastNibble $ pack "asdf") `shouldBe` (6)

    describe "word32ToDecimal" $ do
        it "converts 32bit word to decimal string, mod 1000000" $ do
            (word32ToDecimal (fromIntegral 40010108)) `shouldBe` (pack "010108")

    describe "mm" $ do
        it "converts 32bit word to integer" $ do
            (mm (fromIntegral 3000000011)) `shouldBe` 11

    describe "padL" $ do
        it "adds leading zeros" $ do
            (padL _0 6 (pack "355")) `shouldBe` (pack "000355")

    describe "mask31" $ do
        it "sets first bit to zero" $ do
            (mask31 (toEnum 3147483648)) `shouldBe` (toEnum 1000000000)

    describe "bytesToWord32" $ do
        it "converts 4 byte ByteArray to Word32" $ do
            (bytesToWord32 $ BA.pack (Prelude.replicate 4 (fromIntegral $ ord 'a'))) `shouldBe` (toEnum 1633771873 :: Word32)

    describe "doIt" $ do
        it "does it" $ do
            (doIt (fromNanoSecs 1633771873000000000) (pack "hello world")) `shouldBe` (BSW.pack [0x07, 0xb2, 0x88, 0xa6, 0x39, 0x5f, 0x3f, 0x9a, 0xfc, 0xa6, 0x0c, 0x2d, 0x9a, 0xc7, 0x8b, 0xca, 0xf5, 0x73, 0x31, 0x2f])

    describe "timeblock" $ do
        it "does it" $ do
            (timeblock (fromNanoSecs (34359738360000000055))) `shouldBe` (pack "\0\0\0\0DDDD")
