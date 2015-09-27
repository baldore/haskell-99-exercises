import           Test.Hspec
import           Data.List

-- Problem 11
-- Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.
data Repeated a = Multiple Int a | Single a deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [Repeated a]
encodeModified = map convertToRepeated . group
  where
    convertToRepeated repeated
      | length repeated == 1 = Single (head repeated)
      | otherwise            = Multiple (length repeated) (head repeated)

main :: IO()
main = hspec $
  describe "99-exercises.11 = Modified run-length encoding" $
    it "should return the element as a singleton if there's no repetitions" $
      encodeModified "aaaabccaadeeee" `shouldBe`
      [ Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e' ]
