import Test.Hspec

-- Problem 4
-- Find the length of a list
type Length = Int

myLength :: [a] -> Length
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

main :: IO()
main = hspec $
  describe "99-exercises.4 = length of a list" $
    it "should return the number of elemnents of a list" $ do
      myLength [1..20] `shouldBe` (20 :: Int)
      myLength "Hello, world!" `shouldBe` (13 :: Int)
      myLength "" `shouldBe` (0 :: Int)
