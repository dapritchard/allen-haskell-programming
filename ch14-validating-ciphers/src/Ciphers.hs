module Ciphers where

import Data.Char ( chr, isLower, isUpper, ord )

caesar :: Int -> String -> String
caesar shiftBy = map (caesarLetter shiftBy)

caesarLetter :: Int -> Char -> Char
caesarLetter shiftBy c
  | isUpper c = shiftLetter shiftBy (ord 'A') c
  | isLower c = shiftLetter shiftBy (ord 'A') c
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
