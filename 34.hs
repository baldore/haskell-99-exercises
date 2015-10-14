
import Test.Hspec

-- Problem 34
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
totient :: Int -> Int
totient m = length $ filter ((==1) . gcd m) [1..m]

main :: IO()
main = hspec $
  describe "99-exercises.34 = Euler's totient function phi(m)" $
    it "should return the number of positive integers that are coprime to m" $ do
      totient 1  `shouldBe` 1
      totient 10 `shouldBe` 4
