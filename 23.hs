import System.Random

selectRandom :: [a] -> Int -> IO [a]
selectRandom ls n = do
  gen <- getStdGen
  return $ take n [ls !! x | x <- randomRs (0, length ls - 1) gen]

-- Problem 23
-- Extract a given number of randomly selected elements from a list.
main :: IO()
main = do
  xs <- selectRandom "abcdefghijk" 3
  print xs
