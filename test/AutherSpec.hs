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

--  describe "convert32" $ do
--      it "decodes base32 char" $ do
--          (fromJust $ convert32 _A) `shouldBe` (0)
--          (fromJust $ convert32 _a) `shouldBe` (0)
--          (fromJust $ convert32 _7) `shouldBe` (31)
--          (fromJust $ convert32 _2) `shouldBe` (26)

--  describe "merge" $ do
--      it "combines 8x5-bits to 40-bits" $ do
--          (merge _A _A _E _3 _A _A _N _B) `shouldBe` (Just $ fromIntegral 5963985)
--          --010110110000000011010001
--          --(merge _A _A) `shouldBe` (Just $ fromIntegral 0)
--          --(merge _N _B) `shouldBe` (Just $ fromIntegral 209)

--  describe "convertAll" $ do
--      it "combines 2 nibles" $ do
--          --(convertAll [_E, _3]) `shouldBe` ([Just $ fromIntegral 91])
--          --(convertAll [_E, _3, _4]) `shouldBe` ([Just $ fromIntegral 91, Nothing])
--          --(convertAll [_N, _B, _S, _W]) `shouldBe` (Prelude.map Just (BSW.unpack $ pack "he"))
--          (convertAll (BSW.unpack $ pack "NBSWY3DPEB3W64TM")) `shouldBe` (Prelude.map Just (BSW.unpack $ pack "hello world"))
