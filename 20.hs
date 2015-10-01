import Test.Hspec

-- Problem 20
-- Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a,[a])
removeAt i (x:xs) | i > 1     = (xz,x:yz)
                  | otherwise = (x,xs)
  where (xz,yz) = removeAt (i-1) xs

main :: IO()
main = hspec $
  describe "99-exercises.20 = Remove the K'th element from a list" $
    it "should remove the k'th element of a list an return a pair" $
      removeAt 2 "abcd" `shouldBe` ('b',"acd")
