import System.Random

-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.
getRandomUntil :: Int -> Int -> IO [Int]
getRandomUntil n m = do
  gen <- getStdGen
  return $ take n $ randomRs (1,m) gen

main :: IO()
main = do
  xs <- getRandomUntil 6 49
  print xs
