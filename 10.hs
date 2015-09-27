import           Test.Hspec

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement
-- the so-called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number
-- of duplicates of the element E.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack ls@(x:_) = takeWhile (== x) ls : pack(dropWhile (==x) ls)

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\i -> (length i, head i)) . pack

main :: IO()
main = hspec $
  describe "99-exercises.10 = encode" $
    it "should group the elements in pairs based on the lenght" $
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
