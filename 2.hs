
-- file 2.hs
import           Test.Hspec

-- Problem 2
-- Find the last but one element of a list.
lastButOne :: [a] -> a
lastButOne [x,_] = x
-- lastButOne (x:(_:[])) = x
lastButOne (_:xs) = lastButOne xs

main :: IO()
main = hspec $
  describe "99-exercises.2 = Last butone element of a list" $
    it "returns the last but one element of a list" $ do
      lastButOne [1..20] `shouldBe` (19 :: Int)
      lastButOne ['a'..'z'] `shouldBe` 'y'
