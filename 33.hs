import Test.Hspec

-- Problem 33
-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

-- Returs the highest common divisor
myGCD :: Int -> Int -> Int
myGCD a b
  | b == 0    = abs a
  | otherwise = myGCD b (a `mod` b)

isCoprime :: Int -> Int -> Bool
isCoprime a b = myGCD a b == 1

isCoprime' :: Int -> Int -> Bool
isCoprime' a b = gcd a b == 1

main :: IO()
main = hspec $
  describe "99-exercises.33 = coprime numbers" $
    it "should check if two numbers are coprime" $ do
      isCoprime  35 64 `shouldBe` True
      isCoprime' 35 64 `shouldBe` True
