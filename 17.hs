import Test.Hspec

-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split ls p = split' ([], ls) p
  where
    split' (f,s) 0 = (reverse f,s)
    split' (f,x:xs) n = split' (x:f, xs) (n-1)

main :: IO()
main = hspec $
  describe "99-exercises.17 = Split a list" $
    it "should split a list in two splits based on the position without existing helpers" $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")
      split "abcdefghik" 6 `shouldBe` ("abcdef", "ghik")
