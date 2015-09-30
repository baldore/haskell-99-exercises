import           Test.Hspec

-- Problem 14
-- Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

main :: IO()
main = hspec $
  describe "99-exercises.14 = Duplicate elements" $
    it "should duplicate each element in a list" $
      dupli [1, 2, 3] `shouldBe` ([1,1,2,2,3,3] :: [Int])
