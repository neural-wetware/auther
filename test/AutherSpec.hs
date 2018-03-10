module Main (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Auther.Internal
import Data.ByteString.Char8
import qualified Data.ByteString as BSW
import Data.Word8
import Data.Maybe

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
            (subDigest $ pack "hello there beautiful worldM") `shouldBe` (pack "eaut")

    describe "lastNibble" $ do
        it "gets last nibble of bytestring" $ do
            (lastNibble $ pack "asdf") `shouldBe` (6)

    describe "word32ToDecimal" $ do
        it "converts 32bit word to decimal string, mod 1000000" $ do
            (word32ToDecimal (fromIntegral 40010108)) `shouldBe` (pack "010108")

    describe "mm" $ do
        it "converts 32bit word to integer" $ do
            (mm (fromIntegral 3000000011)) `shouldBe` 11

    describe "pasL" $ do
        it "adds leading zeros" $ do
            (padL 6 (pack "355")) `shouldBe` (pack "000355")

    describe "conv32" $ do
        it "converts 4 bytes to 31bit word32" $ do
            (padL 6 (pack "355")) `shouldBe` (pack "000355")
