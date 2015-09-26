import           Test.Hspec

-- Problem 7
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a
-- `flat' list by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten ls = flatten' ls []
  where
    flatten' (Elem x)      acc = x:acc
    flatten' (List [])     acc = acc
    flatten' (List (x:xs)) acc = flatten' x $ flatten' (List xs) acc

main :: IO()
main = hspec $
  describe "99-exercises.7 = Flatten a nested list structure" $
    it "should return the number of elemnents of a list" $ do
      flatten (Elem 5) `shouldBe` [5]
      flatten (List ([] :: [NestedList Int])) `shouldBe` []
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5, List[]]]) `shouldBe` [1,2,3,4,5]
