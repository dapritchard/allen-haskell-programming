module CiphersTest where

import Ciphers ( caesar, vigenere )
import Test.Hspec ( describe, hspec, it, shouldBe )
import Test.QuickCheck ( Testable(..) )

main :: IO ()
main = hspec $ do

  describe "test caesar" $ do

    describe "empty string" $ do
      it "shift 2" $ do
        caesar 2 ""`shouldBe` ""

    describe "no shift" $ do
      it "shift 0" $ do
        caesar 0 fox`shouldBe` fox
      it "shift 52" $ do
        caesar 52 fox`shouldBe` fox
      it "shift -52" $ do
        caesar (-52) fox`shouldBe` fox

    describe "some shift" $ do
      it "shift 2" $ do
        caesar 2 fox `shouldBe` "VJG swkem dtqyp hqz lworgf qxgt vjg ncba fqi"
      it "shift -2" $ do
        caesar (-2) fox
          `shouldBe`
          "RFC osgai zpmul dmv hskncb mtcp rfc jyxw bme"

fox :: String
fox = "THE quick brown fox jumped over the lazy dog"
