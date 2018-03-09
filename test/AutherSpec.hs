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

--  describe "convertAll" $ do
--      it "combines 2 nibles" $ do
--          --(convertAll [_E, _3]) `shouldBe` ([Just $ fromIntegral 91])
--          --(convertAll [_E, _3, _4]) `shouldBe` ([Just $ fromIntegral 91, Nothing])
--          --(convertAll [_N, _B, _S, _W]) `shouldBe` (Prelude.map Just (BSW.unpack $ pack "he"))
--          (convertAll (BSW.unpack $ pack "NBSWY3DPEB3W64TM")) `shouldBe` (Prelude.map Just (BSW.unpack $ pack "hello world"))
