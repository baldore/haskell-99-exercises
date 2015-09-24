-- file 5.hs
import           Test.Hspec

-- Problem 5
-- Reverse a list
reverse' :: [a] -> [a]
reverse' list = accReverse list []
  where
    accReverse [] acc = acc
    accReverse (x:xs) acc = accReverse xs (x : acc)

main :: IO()
main = hspec $
  describe "99-exercises.5 = Reverse" $
    it "should reverse a list" $ do
      reverse' [1..20] `shouldBe` ([20,19..1] :: [Int])
      reverse' "hello world" `shouldBe` "dlrow olleh"
