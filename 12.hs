import           Test.Hspec

-- Problem 12
-- Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.
data Repeated a = Single a | Multiple Int a

decodeModified :: [Repeated a] -> [a]
decodeModified = concatMap getDecoded
  where
    getDecoded (Single v) = [v]
    getDecoded (Multiple r v) = replicate r v

main :: IO()
main = hspec $
  describe "99-exercises.12 = Decode a run-length encoded list" $
    it "should return the decoded list" $
      decodeModified
        [ Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e' ]
      `shouldBe`
        "aaaabccaadeeee"
