import Test.Hspec

-- Problem 32
-- Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
myGCD :: Int -> Int -> Int
myGCD a b
  | b == 0    = abs a
  | otherwise = myGCD b (a `mod` b)

main :: IO()
main = hspec $
  describe "99-exercises.32 = get the greatest common divisor of two integer numbers" $
    it "should return the common divisor of 2 numbers" $ do
      myGCD  36   63  `shouldBe` 9
      myGCD (-3) (-6) `shouldBe` 3
      myGCD (-3)  6   `shouldBe` 3
