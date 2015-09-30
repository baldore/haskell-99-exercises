import Test.Hspec

-- Problem 18
-- Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice ls 1 n = take n ls
slice (_:xs) i k = slice xs (i-1) (k-1)

main :: IO()
main = hspec $
  describe "99-exercises.18 = slice part of list" $
    it "should return part of the list" $
      slice "abcdefghij" 3 7 `shouldBe` "cdefg"
