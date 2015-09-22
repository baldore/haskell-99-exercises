-- file 1.hs
import           Control.Exception (evaluate)
import           Test.Hspec

-- Problem 1
-- Find the last element of a list.
last' :: [a] -> a
last' [] = error "Please provide a list with at least one element"
last' [x] = x
last' (_:xs) = last' xs

main :: IO()
main = hspec $
  describe "99-exercises.1 = Last element of list" $ do
    it "returns the last element of a list" $ do
      last' [1..20] `shouldBe` (20 :: Int)
      last' "hello world" `shouldBe` 'd'

    it "throws an exception when the list is empty" $
      evaluate (last' []) `shouldThrow` anyException
