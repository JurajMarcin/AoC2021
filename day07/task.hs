import Data.Functor
import Data.List
import Debug.Trace

-- Input

loadCrabs :: String -> IO [Int]
loadCrabs file = readFile file <&> parser ""
    where
        parser :: String -> String -> [Int]
        parser "" []     = []
        parser n  []     = [read (reverse n) :: Int]
        parser n  (x:xs) = if x == ','
                              then (read (reverse n) :: Int) : parser "" xs
                              else parser (x : n) xs


-- Task 1

median :: [Int] -> Int
median list = let mid = length list `div` 2 in sort list !! mid

optimumFuelCost1 :: [Int] -> Int
optimumFuelCost1 list = fuelCostToPos (median list) list
    where
        fuelCostToPos :: Int -> [Int] -> Int
        fuelCostToPos _ []     = 0
        fuelCostToPos m (x:xs) = abs (m - x) + fuelCostToPos m xs


-- Task 2

mean :: Fractional a => [Int] -> a
mean list = fromIntegral (sum list) / fromIntegral (length list)

optimumFuelCost2 :: [Int] -> Int
optimumFuelCost2 list =
        uncurry min $ fuelCostToPos (floor listMean, ceiling listMean) list
    where
        listMean = mean list
        fuelCostToPos :: (Int, Int) -> [Int] -> (Int, Int)
        fuelCostToPos _ []              = (0, 0)
        fuelCostToPos m@(m1, m2) (x:xs) =
            let (s1, s2) = fuelCostToPos m xs in
                (aritSum (abs (m1 - x)) + s1, aritSum (abs (m2 - x)) + s2)
        aritSum :: Int -> Int
        aritSum n = (1 + n) * n `div` 2


-- Main

main :: IO ()
main = do
    crabs <- loadCrabs "input.txt"
    print $ optimumFuelCost1 crabs
    print $ optimumFuelCost2 crabs
