{-# HLINT ignore "Avoid reverse" #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Redundant $" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module UsingQuickCheckTest where

import Data.List ( sort )
import UsingQuickCheck
import Test.Hspec ( hspec, it, shouldNotBe )
import Test.QuickCheck ( Testable(..), NonZero (..) )

main :: IO ()
main = hspec $ do
  -- Exercise 1
  it "twice of half a number is an identity for Doubles" $ do
    property prop_halfIdentityDouble
  -- Exercise 2
  it "sort works for Integers" $ do
    property prop_SortInteger
  -- Exercise 3
  it "Integers are associative under addition" $ do
    property prop_plusAssociativeInteger
  it "Integers are commutative under addition" $ do
    property prop_plusCommutativeInteger
  -- Exercise 4
  it "Integers are associative under multiplication" $ do
    property prop_multiplyAssociativeInteger
  it "Integers are commutative under multiplication" $ do
    property prop_multiplyCommutativeInteger
  -- Exercise 5
  it "quot and rem identity for Integers" $ do
    property prop_quotRemIdentityInteger
  it "div and mod identity for Integers" $ do
    property prop_divModIdentityInteger
  -- Exercise 6
  it "Exponentiation is not associative" $ do
    lparenTwoPowThreeRparenPowFour `shouldNotBe` twoPowLparenThreePowFourRparen
  it "Exponentiation is not commutative" $ do
    twoPowThree `shouldNotBe` threePowTwo
  -- Exercise 7
  it "reverse and reverse identity for Integers" $ do
    property prop_revRevIdentityIntegers
  -- Exercise 8
  it "dollar operator is no-operation when squaring Integers" $ do
    property prop_DollarIsNoOpSquareInt
  -- Exercise 9
  it "foldr with cons postpends a list to another list for Integers" $ do
    property prop_FoldrConsIsPostpendInteger
  it "foldr with binary concat is list concat for Integers" $ do
    property prop_FoldrBinaryConcatIsListConcatInteger
  -- Exercise 10
  it "lenth of `take n xs` is not `n` if `length xs` is less than `n`" $ do
    lengthTakeN intVal2 length1List `shouldNotBe` intVal2
  -- Exercise 11
  it "read and show identity for lists of Integers" $ do
    property prop_showReadListInteger


{- 1. page 856 -----------------------------------------------------------------

-- for a function
half x = x / 2

-- this property should hold
halfIdentity = (*2) . half
-}
prop_halfIdentityDouble :: Double -> Bool
prop_halfIdentityDouble = checkHalfIdentity

checkHalfIdentity :: (Fractional a, Eq a) => a -> Bool
checkHalfIdentity x = halfIdentity x == x

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half


{- 2. page 856 -----------------------------------------------------------------

import Data.List (sort)

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)
-}
prop_SortInteger :: [Integer] -> Bool
prop_SortInteger = checkSort

checkSort :: (Ord a) => [a] -> Bool
checkSort = listOrdered . sort

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _) = (Just y, x >= y)


{- 3. pages 856-857 ------------------------------------------------------------

Now we’ll test the associative and commutative properties
of addition:

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

Keep in mind these properties won’t hold for types based
on IEEE-754 floating point numbers, such as Float or
Double.
-}
prop_plusAssociativeInteger :: Integer -> Integer -> Integer -> Bool
prop_plusAssociativeInteger = plusAssociative

prop_plusCommutativeInteger :: Integer -> Integer -> Bool
prop_plusCommutativeInteger = plusCommutative

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x


{- 4. page 857 -----------------------------------------------------------------

Now do the same for multiplication.
-}
prop_multiplyAssociativeInteger :: Integer -> Integer -> Integer -> Bool
prop_multiplyAssociativeInteger = multiplyAssociative

prop_multiplyCommutativeInteger :: Integer -> Integer -> Bool
prop_multiplyCommutativeInteger = multiplyCommutative

multiplyAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multiplyAssociative x y z = x * (y * z) == (x * y) * z

multiplyCommutative :: (Eq a, Num a) => a -> a -> Bool
multiplyCommutative x y = x * y == y * x


{- 5. page 857 -----------------------------------------------------------------

We mentioned in one of the first chapters that there are
some laws involving the relationship of quot and rem and
div and mod. Write QuickCheck tests to prove them.

-- quot rem
(quot x y)*y + (rem x y) == x

(div x y)*y + (mod x y) == x
-}
prop_quotRemIdentityInteger :: Integer -> NonZero Integer -> Bool
prop_quotRemIdentityInteger = quotRemIdentity

prop_divModIdentityInteger :: Integer -> NonZero Integer -> Bool
prop_divModIdentityInteger = divModIdentity

quotRemIdentity :: Integral a => a -> NonZero a -> Bool
quotRemIdentity x (NonZero y) = quot x y * y + rem x y == x

divModIdentity :: Integral a => a -> NonZero a -> Bool
divModIdentity x (NonZero y) = div x y * y + mod x y == x


{- 6. page 857 -----------------------------------------------------------------

Is (^) associative? Is it commutative? Use QuickCheck to see
if the computer can contradict such an assertion.
-}
lparenTwoPowThreeRparenPowFour :: Integer
lparenTwoPowThreeRparenPowFour = ((2::Integer) ^ (3::Integer)) ^ (4::Integer)

twoPowLparenThreePowFourRparen :: Integer
twoPowLparenThreePowFourRparen = (2 :: Integer) ^ ((3 :: Integer) ^ (4 :: Integer))

twoPowThree :: Integer
twoPowThree = (2::Integer) ^ (3::Integer)

threePowTwo :: Integer
threePowTwo = (3::Integer) ^ (2::Integer)


{- 7. page 857 -----------------------------------------------------------------

Test that reversing a list twice is the same as the identity
of the list:

reverse . reverse == id
-}
prop_revRevIdentityIntegers :: [Integer] -> Bool
prop_revRevIdentityIntegers = checkRevRevIdentity

checkRevRevIdentity :: Eq a => [a] -> Bool
checkRevRevIdentity x = reverse (reverse x) == x


{- 8. pages 857-858 ------------------------------------------------------------

Write a property for the definition of ($).

f $ a = f a

f . g = \x -> f (g x)
-}
prop_DollarIsNoOpSquareInt :: Integer -> Bool
prop_DollarIsNoOpSquareInt = checkDollarIsNoOp (^ (2::Integer))

checkDollarIsNoOp :: Eq b => (a -> b) -> a -> Bool
checkDollarIsNoOp f x = (f $ x) == f x


{- 9. page 858 -----------------------------------------------------------------

See if these two functions are equal:

foldr (:) == (++)

foldr (++) [] == concat
-}
prop_FoldrConsIsPostpendInteger :: [Integer] -> [Integer] -> Bool
prop_FoldrConsIsPostpendInteger = checkFoldrConsIsPostpend

prop_FoldrBinaryConcatIsListConcatInteger :: [[Integer]] -> Bool
prop_FoldrBinaryConcatIsListConcatInteger = checkFoldrBinaryConcatIsListConcat

checkFoldrConsIsPostpend :: (Eq a) => [a] -> [a] -> Bool
checkFoldrConsIsPostpend xs ys = foldr (:) xs ys == ys ++ xs

checkFoldrBinaryConcatIsListConcat :: (Eq a) => [[a]] -> Bool
checkFoldrBinaryConcatIsListConcat xs = foldr (++) [] xs == concat xs


{- 10. page 858 -----------------------------------------------------------------

Hm. Is that so?

f n xs = length (take n xs) == n
-}
lengthTakeN :: Int -> [a] -> Int
lengthTakeN n xs = length (take n xs)

intVal2 :: Int
intVal2 = 2

length1List :: [Int]
length1List = [1]


{- 11. page 858 ----------------------------------------------------------------

Finally, this is a fun one. You may remember we had you
compose read and show one time to complete a “round
trip.” Well, now you can test that it works:

f x = (read (show x)) == x
-}
prop_showReadListInteger :: [Integer] -> Bool
prop_showReadListInteger = checkReadShow

checkReadShow :: (Eq a, Read a, Show a) => a -> Bool
checkReadShow x = read (show x) == x
