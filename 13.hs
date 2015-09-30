import           Test.Hspec

-- Problem 13
-- Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method
-- directly. I.e. don't explicitly create the sublists containing
-- the duplicates, as in problem 9, but only count them. As in
-- problem P11, simplify the result list by replacing the singleton
-- lists (1 X) by X.
data Repeated a = Single a | Multiple Int a deriving (Eq, Show)

preEncode :: Eq a => [a] -> [(Int, a)]
preEncode = foldr getPair []
  where
    getPair x [] = [(1,x)]
    getPair x ((a,b):ys)
      | x == b    = (a+1,b):ys
      | otherwise = (1,x):(a,b):ys

encodeDirect :: Eq a => [a] -> [Repeated a]
encodeDirect xs = map pairToRepeated (preEncode xs)
  where
    pairToRepeated (1, x) = Single x
    pairToRepeated (r, x) = Multiple r x

main :: IO()
main = hspec $
  describe "99-exercises.13 = Direct encoding" $ do
    it "should convert a list into a list of pairs before encoding" $
      preEncode "aaabbc" `shouldBe` [(3,'a'), (2,'b'), (1,'c')]
    it "should encode a list without using sublists" $
      encodeDirect
        "aaaabccaadeeee"
      `shouldBe` [
        Multiple 4 'a',
        Single 'b',
        Multiple 2 'c',
        Multiple 2 'a',
        Single 'd',
        Multiple 4 'e'
      ]
