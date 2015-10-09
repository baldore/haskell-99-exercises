import System.Random

-- Problem 25
-- Generate a random permutation of the elements of a list.

-- Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a,[a])
removeAt i (x:xs) | i > 1     = (xz,x:yz)
                  | otherwise = (x,xs)
  where (xz,yz) = removeAt (i-1) xs

rndPermutationG :: RandomGen g => [a] -> g -> [a]
rndPermutationG [] _   = []
rndPermutationG ls gen =
  let (i,g)  = randomR (0, length ls - 1) gen
      (x,xs) = removeAt i ls
  in x : rndPermutationG (reverse xs) g

rndPermutation :: [a] -> IO [a]
rndPermutation ls = do
  gen <- getStdGen
  return $ rndPermutationG ls gen

main :: IO()
main = do
  xs <- rndPermutation "abcdefg"
  print xs
