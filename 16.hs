import Test.Hspec

-- Problem 16
-- Drop every N'th element from a list.
dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery ls n = reverse $ get' ls [] n n
  where
    get' [] acc _ _ = acc
    get' (x:xs) acc t i
      | i == 1    = get' xs acc t t
      | otherwise = get' xs (x:acc) t (i-1)

main :: IO()
main = hspec $
  describe "99-exercises.16 = Drop every N'th element from a list" $
    it "should drop each n'th element in a list" $
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
