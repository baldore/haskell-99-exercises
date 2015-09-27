import           Test.Hspec

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack ls@(x:_) = takeWhile (== x) ls : pack(dropWhile (==x) ls)

main :: IO()
main = hspec $
  describe "99-exercises.9 = Pack consecutive similar elements" $
    it "should remove the consecutive repeated elements" $
      pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]
