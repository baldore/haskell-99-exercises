import Test.Hspec

-- Problem 22
-- Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range l h
  | l == h    = [h]
  | otherwise = l:range (l+1) h

main :: IO()
main = hspec $
  describe "99-exercises.22 = Create a range between elements" $
    it "should return a range between elements" $
      range 4 9 `shouldBe` [4,5,6,7,8,9]
