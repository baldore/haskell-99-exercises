import           Test.Hspec

-- Reverse Polish Notation Resolver
-- For more info, check: https://en.wikipedia.org/wiki/Reverse_Polish_notation
solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl solve [] . words
  where
    solve (x:y:xs) "+" = (y + x):xs
    solve (x:y:xs) "-" = (y - x):xs
    solve (x:y:xs) "*" = (y * x):xs
    solve xs       num = read num:xs

main :: IO()
main = hspec $
  describe "Reverse Polish Notation" $
    it "should return the result of a reverse polish notation formula" $ do
      solveRPN "10 4 3 + 2 * -" `shouldBe` ((-4) :: Int)
      solveRPN "2 3 +" `shouldBe` (5 :: Int)
      solveRPN "90 3 -" `shouldBe` (87 :: Int)
      solveRPN "90 34 12 33 55 66 + * - + -" `shouldBe` (4037 :: Int)
