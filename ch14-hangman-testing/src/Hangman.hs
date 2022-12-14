{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use when" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hangman where

import Control.Monad (forever)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


-- Step Two: Generating a word list --------------------------------------------

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
      in  l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord


-- Step Three: Making a puzzle -------------------------------------------------

data Puzzle = Puzzle
  String        -- the word we’re trying to guess
  [Maybe Char]  -- the characters we’ve filled in so far
  [Char]        -- the letters we’ve guessed so far
  deriving (Eq)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
      ++ " Guessed so far: "
      ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (replicate (length word) Nothing) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discovered guessed) c =
  Puzzle word newDiscovered (c:guessed)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newDiscovered = zipWith (zipper c) word discovered

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do

  putStrLn $ "Your guess was: " ++ [guess]
  case ( charInWord puzzle guess
       , alreadyGuessed puzzle guess) of

    (_, True) -> do
       putStrLn "You already guessed that character, pick something else!"
       return puzzle

    (True, _) -> do
       putStrLn
         "This character was in the word, filling in the word accordingly"
       return (fillInCharacter puzzle guess)

    (False, _) -> do
        putStrLn "This character wasn't in the word, try again."
        return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if length guessed > 7 then do
    putStrLn "You lose!"
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ discovered _) =
  if all isJust discovered
  then do
    putStrLn "You win!"
    exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"
