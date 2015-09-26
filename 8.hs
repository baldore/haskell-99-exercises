import           Test.Hspec
import           Data.List

-- Problem 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single
-- copy of the element. The order of the elements should not be changed.
compress :: (Eq a) => [a] -> [a]

-- With Guards
-- Take the head with pattern matching. Then take the head of the tail without
-- losing the reference of the tail to use it in the next call
compress (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress ys = ys

 -- Solution using group (Data.List function)
-- compress ls = map head $ group ls

-- Solution with Concat - Own conclusion
-- compress []     = []
-- compress (x:xs) = comp' xs x [x]
--   where
--     comp' []     lst acc = acc
--     comp' (x:xs) lst acc = comp' xs x (if x == lst then acc else acc ++ [x])

main :: IO()
main = hspec $
  describe "99-exercises.8 = Remove consecutive duplicated elements" $
    it "should remove the consecutive repeated elements" $ do
      compress "" `shouldBe` ""
      compress "aaaabccaadeeee" `shouldBe` "abcade"
