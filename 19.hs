import Test.Hspec

-- Problem 19
-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate ls p
  | p < 0     = rotate ls (length ls + p)
  | otherwise = drop p ls ++ take p ls

main :: IO()
main = hspec $
  describe "99-exercises.19 = Rotate a list n places" $
    it "should drop each n'th element in a list" $ do
      rotate "abcdefgh" 3 `shouldBe` "defghabc"
      rotate "abcdefgh" (-2) `shouldBe` "ghabcdef"
