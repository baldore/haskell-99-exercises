import Test.Hspec

-- Problem 31
-- Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime n = primes == 1
  where primes = length $ filter ((==) 0 . mod n) [2..n]

main :: IO()
main = hspec $
  describe "99-exercises.31 = is it a prime number?" $
    it "should determine if a number is prime" $ do
      isPrime 7  `shouldBe` True
      isPrime 10 `shouldBe` False
      isPrime 13 `shouldBe` True
