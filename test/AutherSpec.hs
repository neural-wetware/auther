module Main (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Auther.Internal
import System.Clock
import Data.ByteString.Char8
import qualified Data.ByteString as BSW
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Word8
import Data.Word
import Data.Maybe
import Data.Binary.Get -- TODO use lazy equivalent? cereal?
import Data.Binary (encode)
import Data.ByteString.Conversion

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "base32decode" $ do
        it "decodes base32 string" $ do
            pendingWith "fix base32decode bug"
            --(base32decode $ pack "NBSWY3DPEB3W64TMMQ") `shouldBe` (Just $ pack "hello world")

-- this can be done by sending random secrets!?
--      prop "equals the unit value" $
--          \ x -> husk == x

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

    describe "makeWord" $ do
        it "converts 4 byte ByteString to Word32" $ do
            (makeWord (pack "aaaa")) `shouldBe` (toEnum 1633771873 :: Word32)

    describe "test runGet" $ do
        it "sets first bit to zero" $ do
            (runGet getWord32be (LBSC.pack "aaaa")) `shouldBe` (toEnum 1633771873 :: Word32)

    describe "doIt" $ do
        it "does it" $ do
            (doIt (fromNanoSecs 1633771873000000000) (pack "NBSWY3DPEB3W64TMMQ")) `shouldBe` (pack "ffff")

    describe "timeblock" $ do
        it "does it" $ do
            (timeblock (fromNanoSecs (34359738360 * 1000000000))) `shouldBe` (pack "\0\0\0\0DDDD")
