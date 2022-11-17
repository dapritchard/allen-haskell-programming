module RandomGenTest where

import Test.Hspec ( hspec, it )
import Test.QuickCheck ( Arbitrary(..), Gen, Testable(..), frequency, oneof )

{- Make a Gen random generator for the datatype: page 860 ----------------------

We demonstrated in the chapter how to make Gen generators
for different datatypes. We are so certain you enjoyed that, we
are going to ask you to do it for some new datatypes:
-}

main :: IO ()
main = hspec $ do
  it "Silly check of derived `==` instance for Fool" $ do
    property $ \x -> x == (x::Fool)

data Fool = Fulse | Frue
  deriving (Eq, Show)

-- Note that we can only choose one distribution (e.g. either `genFoolUnif` or
-- `genFoolNonunif`) but if we want another distribution then we need to create
-- a different type
instance Arbitrary Fool where
  arbitrary = genFoolUnif



{- 1. Page 860 -----------------------------------------------------------------

Equal probabilities for each.

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)
-}
genFoolUnif :: Gen Fool
genFoolUnif = oneof [return Fulse, return Frue]


{- 2. Page 860 -----------------------------------------------------------------

2/3s chance of Fulse, 1/3 chance of Frue.

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)
-}
genFoolNonunif :: Gen Fool
genFoolNonunif =
  frequency
    [ (2, return Fulse)
    , (1, return Frue)
    ]
