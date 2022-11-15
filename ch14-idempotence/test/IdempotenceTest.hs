module IdempotenceTest where

import Test.Hspec ( hspec, it, shouldNotBe )

{- Idempotence: pages 859-860 --------------------------------------------------

Idempotence refers to a property of some functions in which
the result value does not change beyond the initial application.
If you apply the function once, it returns a result, and applying
the same function to that value won’t ever change it. You might
think of a list that you sort: once you sort it, the sorted list will
remain the same after applying the same sorting function to
it. It’s already sorted, so new applications of the sort function
won’t change it.

Use QuickCheck and the following helper functions to demonstrate
idempotence for the following:

twice f = f . f
fourTimes = twice . twice
-}

main :: IO ()
main = hspec $ do
  it "Exponentiation is not commutative" $ do
    1 `shouldNotBe` (2::Integer)

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice


{- 1. Page 859 -----------------------------------------------------------------

f x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)
-}


{- 2. Page 860 -----------------------------------------------------------------

f' x =
  (sort x
  == twice sort x)
  &&
  (sort x
  == fourTimes sort x)
-}
