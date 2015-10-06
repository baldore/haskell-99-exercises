import Test.Hspec

-- Taken from http://learnyouahaskell.com/functionally-solving-problems
-- Find the cheapest path from Heathrow to london based on this input:

-- 50 10 30 5 90 20 40 2 25 10 8 0
-- The input is grouped in 3 [a,b,c] for each cross.

-- Section: Represents each section of the via [A B C]
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

-- Used to represent each step
data Label = A | B | C deriving (Show, Eq)
type Path = [(Label, Int)]

-- Temp builder
heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

-- Elements in new paths are prepended with : in order to improve performance.
-- At the end, they will be reversed.
calculateShortestPath :: (Path, Path) -> Section -> (Path, Path)
calculateShortestPath (pathA, pathB) (Section a b c) =
  let totalA        = sum $ map snd pathA
      totalB        = sum $ map snd pathB
      forwardPriceA = totalA + a
      forwardPriceB = totalB + b
      crossPriceToA = totalB + b + c
      crossPriceToB = totalA + a + c
      newPathToA    = if forwardPriceA <= crossPriceToA
                        then (A,a):pathA
                        else (C,c):(B,b):pathB
      newPathToB    = if forwardPriceB <= crossPriceToB
                        then (B,b):pathB
                        else (C,c):(A,a):pathA
  in (newPathToA, newPathToB)

-- Get the shortest path of the road system
getShortestPath :: RoadSystem -> (Path, Int)
getShortestPath roadSystem =
  let (pathA, pathB) = foldl calculateShortestPath ([],[]) roadSystem
      getTotal = sum . map snd
      shortestPath = if getTotal pathA <= getTotal pathB then pathA else pathB
  in (reverse shortestPath, getTotal shortestPath)

main :: IO()
main = hspec $
  describe "Road from heathrowToLondon" $ do
    it "should calculate the shortest path in each section for A and B" $
      calculateShortestPath ([], []) (head heathrowToLondon) `shouldBe` ([(C,30),(B,10)],[(B,10)])
    it "should get the sections " $
      getShortestPath heathrowToLondon `shouldBe` ([(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)], 75)
