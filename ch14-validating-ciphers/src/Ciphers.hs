module Ciphers where

import Data.Char ( chr, isAsciiLower, isAsciiUpper, ord )

caesar :: Int -> String -> String
caesar shiftBy = map (caesarLetter shiftBy)

vigenere :: String -> String -> String
vigenere [] message      = message
vigenere keyword message = go offsets message
  where

    offsets = map calcOffset keyword
      where
        calcOffset c
          | isAsciiUpper c = ord c - ord 'A'
          | isAsciiLower c = ord c - ord 'a'
          | otherwise = 0

    go _ []          = []
    go [] ms         = go offsets ms
    go (o:os) (m:ms) = caesarLetter o m : go os ms

caesarLetter :: Int -> Char -> Char
caesarLetter shiftBy c
  | isAsciiUpper c = shiftLetter shiftBy (ord 'A') c
  | isAsciiLower c = shiftLetter shiftBy (ord 'a') c
  | otherwise = c

shiftLetter :: Int -> Int -> Char -> Char
shiftLetter shiftBy offset letter =
  let normLetter = ord letter - offset
      shiftedNormLetter = shiftNormLetter shiftBy normLetter
  in  chr $ shiftedNormLetter + offset

shiftNormLetter :: Int -> Int -> Int
shiftNormLetter shiftBy normLetter =
  let shifted = (shiftBy + normLetter) `rem` 26
  in  if shifted >= 0 then shifted else shifted + 26
