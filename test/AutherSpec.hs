module Main (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Base32Decode
import Base32Decode.Internal
import Data.ByteString.Char8
import Data.Word8
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "base32decode" $ do
        it "decodes base32 string" $ do
            pendingWith "fix base32decode bug"
            --(fromJust $ base32decode $ pack "d1jprv3f41vpywkccg") `shouldBe` (fromJust $ base32decode $ pack "hello world")

-- this can be done by sending random secrets!?
--      prop "equals the unit value" $
--          \ x -> husk == x

    describe "convert32" $ do
        it "decodes base32 char" $ do
            (fromJust $ convert32 _A) `shouldBe` (0)
            (fromJust $ convert32 _7) `shouldBe` (31)
            (fromJust $ convert32 _2) `shouldBe` (26)
