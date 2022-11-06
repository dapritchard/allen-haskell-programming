module WordNumber where

import Data.List (intersperse)

-- It'd probably be better to return a Maybe String for the case where don't get
-- a number between 0 and 9 (inclusive), but it's done this way to conform with
-- the book's type
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "undefined"

digits :: Int -> [Int]
digits n = reverse (go n)
 where
  go n
    | n < 0       = digits (-n)
    | n < 9       = [n]
    | otherwise   =  n `mod` 10 : go (n `div` 10)

wordNumber :: Int -> String
wordNumber n
  | n < 0 = "negative " ++ go n
  | otherwise = go n
 where
  -- We could simplify this by using `intercalate` here, but we'll stick to
  -- `concat` with `intersperse` to match the book's suggestions
  go n = concat $ intersperse "-" (map digitToWord (digits n))
