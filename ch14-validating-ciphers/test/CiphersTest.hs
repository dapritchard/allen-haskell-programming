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

    describe "property: length is unchanged" $ do
      it "check length invariant" $ do
        property prop_caesarLengthIsUnchanged

    describe "property: -1 times `shiftBy` is caesar inverse" $ do
      it "check caesar inverse" $ do
        property prop_caesarRoundtrip

  describe "test vigenere" $ do

    describe "empty keyword or message" $ do
      it "empty keyword is no-op for empty message" $ do
        vigenere "" "" `shouldBe` ""
      it "empty keyword is no-op for nonempty message" $ do
        vigenere "" fox `shouldBe` fox
      it "nonempty keyword with empty message results in an empty message" $ do
        vigenere "c" "" `shouldBe` ""

    describe "keyword of all 'a's or non-Ascii alphabetical" $ do
      it "'a' is no-op" $ do
        vigenere "a" fox `shouldBe` fox
      it "'aaaa' is no-op" $ do
        vigenere "a" fox `shouldBe` fox
      it "'?*&^' is no-op" $ do
        vigenere "?*&^" fox `shouldBe` fox

    describe "basic functionality" $ do
      it "'b' keyword" $ do
        vigenere "b" fox `shouldBe` caesar 1 fox
      it "'c' keyword" $ do
        vigenere "c" fox `shouldBe` caesar 2 fox
      it "'abc' keyword" $ do
        vigenere "abc" fox
          `shouldBe`
          -- "THE quick brown fox jumped over the lazy dog"  <- caesar 0 fox
          -- "UIF rvjdl cspxo gpy kvnqfe pwfs uif mbaz eph"  <- caesar 1 fox
          -- "VJG swkem dtqyp hqz lworgf qxgt vjg ncba fqi"  <- caesar 2 fox
          "TIG rwidm ctoxp gqx lunree owgr vhf lbby foh"
      it "'ABC' keyword" $ do
        vigenere "ABC" fox
          `shouldBe`
          vigenere "abc" fox

    describe "property: repeated keyword is same as single keyword" $ do
      it "check repeated keyword" $ do
        property prop_vigenereRepeatKeywordSameAsSingle

fox :: String
fox = "THE quick brown fox jumped over the lazy dog"

prop_caesarLengthIsUnchanged :: Int -> String -> Bool
prop_caesarLengthIsUnchanged shiftBy message =
  length (caesar shiftBy message) == length message

prop_caesarRoundtrip :: Int -> String -> Bool
prop_caesarRoundtrip shiftBy message =
  let encrypted = caesar shiftBy message
      decrypted = caesar (-shiftBy) encrypted
  in  decrypted == message

prop_vigenereRepeatKeywordSameAsSingle :: String -> String -> Bool
prop_vigenereRepeatKeywordSameAsSingle keyword message =
  let twice = keyword ++ keyword
  in  vigenere twice message == vigenere keyword message
