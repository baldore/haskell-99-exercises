import Test.Hspec

-- Problem 7
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a
-- `flat' list by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

main :: IO()
main = hspec $
  describe "99-exercises.7 = Flatten a nested list structure" $
    it "should return the number of elemnents of a list" $ do
      flatten (Elem 5) `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]
      flatten (List ([] :: [NestedList Int])) `shouldBe` []
