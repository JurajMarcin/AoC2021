import System.IO

-- Input

loadMeasurements :: String -> IO [Int]
loadMeasurements file = readFile file
    >>= return . map (read :: String -> Int) . words

-- Task

countIncreasing :: Int -> [Int] -> Int
countIncreasing windowSize measurements = counter measurements []
    where
        counter :: [Int] -> [Int] -> Int
        counter [] _ = 0
        counter (m:ms) [] = counter ms [m]
        counter (m:ms) window@(w:ws) =
            if length window == windowSize
                then let newWindow = ws ++ [m] in
                    counter ms newWindow + 
                    (if sum newWindow > sum window then 1 else 0)
                else counter ms (window ++ [m])

-- Main

main :: IO ()
main = do
    measurements <- loadMeasurements "input.txt"
    print $ countIncreasing 1 measurements
    print $ countIncreasing 3 measurements
