module FailureTest where

import Test.Hspec ( hspec, it, shouldNotBe )

main :: IO ()
main = hspec $ do
  it "Exponentiation is not commutative" $ do
    squareIdentity 1000001.1222345 `shouldNotBe` 1000001.1222345


{- pages 858-859 ---------------------------------------------------------------

-- for a function
square x = x * x

-- why does this property not hold?
-- Examine the type of sqrt.
squareIdentity = square . sqrt

Hint: Read about floating point arithmetic and precision if
you’re unfamiliar with it.
-}

square :: Num a => a -> a
square x = x * x

squareIdentity :: Double -> Double
squareIdentity = square . sqrt
