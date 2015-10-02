import Test.Hspec

-- Problem 21
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt el ls 1 = el : ls
insertAt el (x:xs) i = x : insertAt el xs (i-1)

main :: IO()
main = hspec $
  describe "99-exercises.21 = insert in position" $
    it "should insert an element in the specified position" $
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"
