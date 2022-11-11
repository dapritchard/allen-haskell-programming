module UsingQuickCheck where

half :: Fractional a => a -> a
half x = x / 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
 where
  go _ status@(_, False) = status
  go y (Nothing, t) = (Just y, t)
  go y (Just x, _) = (Just y, x >= y)

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x
