import Test.Hspec

-- Problem 15
-- Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli ls t = concatMap (replicate t) ls

main :: IO()
main = hspec $
  describe "99-exercises.15 = Replicate elements in a list" $
    it "should replicate each element in a list" $ do
      repli "abc" 3 `shouldBe` "aaabbbccc"
      repli "abc" 4 `shouldBe` "aaaabbbbcccc"
