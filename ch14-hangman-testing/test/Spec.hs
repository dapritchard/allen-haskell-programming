module Spec where

import Test.Hspec ( describe, hspec, it, shouldBe, shouldReturn )
import Test.QuickCheck ( Testable(..) )
import Hangman ( fillInCharacter, freshPuzzle, handleGuess, Puzzle(..) )

{- Hangman testing: pages 861-862

Next, you should go back to the hangman project from the
previous chapter and write tests. The kinds of tests you can
write at this point will be limited due to the interactive nature
of the game. However, you can test the functions. Focus your
attention on testing the following:

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word
                 filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)

  where zipper s wordChar guessChar =
          if wordChar == s
          then Just wordChar
          else guessChar

        newFilledInSoFar =
          let zd = (zipper c)
          in zipWith zd word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
   (_, True) -> do
      putStrLn "You already guessed that\
                \ character, pick\
                \ something else!"
      return puzzle

   (True, _) -> do
      putStrLn
        "This character was in the\
         \ word, filling in the\
         \ word accordingly"
      return (fillInCharacter puzzle guess)

   (False, _) -> do
       putStrLn "This character wasn't in\
                 \ the word, try again."
       return (fillInCharacter puzzle guess)
-}

main :: IO ()
main = hspec $ do

  describe "test fillInCharacter" $ do

    describe "fresh puzzle" $ do
      it "ensure fresh puzzle with word 'lambda'" $ do
        puzzleLambdaFresh `shouldBe` Puzzle "lambda" sixNothings []

    describe "guess letter not in word" $ do
      it "guess missing letter with no existing misses" $ do
        puzzleLambdaMissesC `shouldBe` Puzzle "lambda" sixNothings "c"
      it "guess missing letter with an existing miss" $ do
        puzzleLambdaMissesCE `shouldBe` Puzzle "lambda" sixNothings "ec"

    describe "guess letter in word" $ do
      it "guess correct letter with no previous correct guesses" $ do
        puzzleLambdaMissesCEGuessedB
          `shouldBe`
          Puzzle
            "lambda"
            [Nothing, Nothing, Nothing, Just 'b', Nothing, Nothing]
            "bec"
      it "guess correct letter with a previous correct guesses" $ do
        puzzleLambdaMissesCEGuessedBA
          `shouldBe`
          Puzzle
            "lambda"
            [Nothing, Just 'a', Nothing, Just 'b', Nothing, Just 'a']
            "abec"

    describe "guess non-alphabetical character" $ do
      it "guess '@'" $ do
        fillInCharacter puzzleLambdaFresh '@'
          `shouldBe`
          Puzzle "lambda" sixNothings "@"

    describe "guess duplicate character" $ do
      it "guess 'b' twice" $ do
        fillInCharacter puzzleLambdaMissesCEGuessedBA 'b'
          `shouldBe`
          Puzzle
            "lambda"
            [Nothing, Just 'a', Nothing, Just 'b', Nothing, Just 'a']
            "babec"

    describe "property: new guess prepends to guessed list" $ do
      it "guess for fresh board" $ do
        property prop_fillInCharacter_guessedListAlwaysPrependsFresh
      it "guess for nonfresh board" $ do
        property prop_fillInCharacter_guessedListAlwaysPrependsNonfresh

    describe "property: new guess never takes away a discovered entry" $ do
      it "guess for fresh board" $ do
        property prop_fillInCharacter_discoveredEntriesNondecreasingFresh
      it "guess for nonfresh board" $ do
        property prop_fillInCharacter_discoveredEntriesNondecreasingNonfresh

  describe "test handleGuess" $ do

    describe "add non-guessed character" $ do
      it "guess for fresh board not in word" $ do
        handleGuess puzzleLambdaFresh 'c'
          `shouldReturn`
          fillInCharacter puzzleLambdaFresh 'c'
      it "guess for fresh board yes in word" $ do
        handleGuess puzzleLambdaFresh 'l'
          `shouldReturn`
          fillInCharacter puzzleLambdaFresh 'l'
      it "guess for nonfresh board not in word" $ do
        handleGuess puzzleLambdaMissesCEGuessedBA 'f'
          `shouldReturn`
          fillInCharacter puzzleLambdaMissesCEGuessedBA 'f'
      it "guess for nonfresh board yes in word" $ do
        handleGuess puzzleLambdaMissesCEGuessedBA 'l'
          `shouldReturn`
          fillInCharacter puzzleLambdaMissesCEGuessedBA 'l'

    describe "add guessed character" $ do
      it "guess for discovered characterd" $ do
        handleGuess puzzleLambdaMissesCEGuessedBA 'b'
          `shouldReturn`
          puzzleLambdaMissesCEGuessedBA
      it "guess for non-discovered characterd" $ do
        handleGuess puzzleLambdaMissesCEGuessedBA 'c'
          `shouldReturn`
          puzzleLambdaMissesCEGuessedBA

sixNothings :: [Maybe a]
sixNothings = replicate 6 Nothing

puzzleLambdaFresh :: Puzzle
puzzleLambdaFresh = freshPuzzle "lambda"

puzzleLambdaMissesC :: Puzzle
puzzleLambdaMissesC = fillInCharacter puzzleLambdaFresh 'c'

puzzleLambdaMissesCE :: Puzzle
puzzleLambdaMissesCE = fillInCharacter puzzleLambdaMissesC 'e'

puzzleLambdaMissesCEGuessedB :: Puzzle
puzzleLambdaMissesCEGuessedB = fillInCharacter puzzleLambdaMissesCE 'b'

puzzleLambdaMissesCEGuessedBA :: Puzzle
puzzleLambdaMissesCEGuessedBA = fillInCharacter puzzleLambdaMissesCEGuessedB 'a'

checkGuessesPrepends :: Puzzle -> Puzzle -> Bool
checkGuessesPrepends (Puzzle _ _ []) _                   = False
checkGuessesPrepends (Puzzle _ _ (_:g1)) (Puzzle _ _ g2) = g1 == g2

prop_fillInCharacter_guessedListAlwaysPrependsFresh ::  Char -> Bool
prop_fillInCharacter_guessedListAlwaysPrependsFresh c =
  checkGuessesPrepends (fillInCharacter puzzleLambdaFresh c) puzzleLambdaFresh

prop_fillInCharacter_guessedListAlwaysPrependsNonfresh ::  Char -> Bool
prop_fillInCharacter_guessedListAlwaysPrependsNonfresh c =
  checkGuessesPrepends
    (fillInCharacter puzzleLambdaMissesCEGuessedBA c)
    puzzleLambdaMissesCEGuessedBA

checkDiscoveredEntriesNondecreasing :: Puzzle -> Puzzle -> Bool
checkDiscoveredEntriesNondecreasing (Puzzle _ d1 _) (Puzzle _ d2 _) =
  (length d1 == length d2)
    &&
    all checkEntry (zip d1 d2)
  where
    checkEntry (Nothing, Just _) = False
    checkEntry (_, _)            = True

prop_fillInCharacter_discoveredEntriesNondecreasingFresh ::  Char -> Bool
prop_fillInCharacter_discoveredEntriesNondecreasingFresh c =
  checkGuessesPrepends (fillInCharacter puzzleLambdaFresh c) puzzleLambdaFresh

prop_fillInCharacter_discoveredEntriesNondecreasingNonfresh ::  Char -> Bool
prop_fillInCharacter_discoveredEntriesNondecreasingNonfresh c =
  checkGuessesPrepends
    (fillInCharacter puzzleLambdaMissesCEGuessedBA c)
    puzzleLambdaMissesCEGuessedBA
