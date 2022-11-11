module UsingQuickCheckTest where

-- import UsingQuickCheck
import Test.Hspec ( hspec, it )
import Test.QuickCheck ( Testable(..) )

main :: IO ()
main = hspec $ do
  it "returns zero for 0" $ do
    property $ \x -> x + 1 > (x :: Int)

-- prop_halfIdentity
