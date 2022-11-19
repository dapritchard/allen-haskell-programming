module Spec where

import Test.Hspec ( describe, hspec, it, shouldBe )
import Hangman

{- Hangman testing: pages 861-862

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

  describe "fresh puzzle" $ do
    it "ensure fresh puzzle with word 'lambda'" $ do
      puzzleLambdaFresh `shouldBe` Puzzle "lambda" [] []

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
