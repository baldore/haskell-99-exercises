import Test.Hspec

-- Problem 16
-- Drop every N'th element from a list.
dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery ls n = get' ls n n
  where
    get' [] _ _     = []
    get' (_:xs) t 1 = get' xs t t
    get' (x:xs) t i = x : get' xs t (i-1)

main :: IO()
main = hspec $
  describe "99-exercises.16 = Drop every N'th element from a list" $
    it "should drop each n'th element in a list" $
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
