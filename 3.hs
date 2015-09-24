import Test.Hspec

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt ls i = ls !! index
  where index = i - 1

main :: IO()
main = hspec $
  describe "99-exercises.3 = k th element of a list" $
    it "returns the element in defined position" $ do
      elementAt [1..10] 8 `shouldBe` (8 :: Int)
      elementAt ['a'..'z'] 4 `shouldBe` 'd'
