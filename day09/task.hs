{-# LANGUAGE TupleSections #-}
import Data.Functor
import Data.List

type Height = Int
type Heatmap = [[Height]]
type Point = (Int, Int)

-- Input

loadHeatmap :: String -> IO Heatmap
loadHeatmap file = readFile file <&> map parse . lines
    where
        parse :: String -> [Int]
        parse "" = []
        parse (x:xs) = (read [x] :: Int) : parse xs


-- Task

findLowPoints :: Heatmap -> [Point]
findLowPoints = fst . flip (findLows 0) (repeat 10)
    where
        findLows :: Int -> Heatmap -> [Height] -> ([Point], [Height])
        findLows _ []             _      = ([], repeat 10)
        findLows i (line:heatmap) lineUp =
            let (lows, lineDown) = findLows (i + 1) heatmap line
                (lineLows, _) = findLowsLine 0 line 10 lineUp lineDown
                in (map (, i) lineLows ++ lows, line)
        findLowsLine :: Int -> [Height] -> Height -> [Height] -> [Height]
            -> ([Int], Height)
        findLowsLine _ []     _    _        _            = ([], 10)
        findLowsLine i (x:xs) left (up:ups) (down:downs) =
            let (lineLows, right) = findLowsLine (i + 1) xs x ups downs in
                if x < left && x < right && x < up && x < down
                    then (i : lineLows, x)
                    else (lineLows, x)


-- Task 1

task1 :: Heatmap -> [Point] -> Int
task1 = calculateRisk 0 0
    where
        calculateRisk :: Int -> Int -> Heatmap -> [Point] -> Int
        calculateRisk x y []             _                 = 0
        calculateRisk x y _              []                = 0
        calculateRisk x y ([]:map)       points            =
            calculateRisk 0 (y + 1) map points
        calculateRisk x y ((h:line):map) points@((px, py):nextPoints) =
            if x == px && y == py
                then h + 1 + calculateRisk (x + 1) y (line:map) nextPoints
                else calculateRisk (x + 1) y (line:map) points


-- Task 2

basinSize :: Heatmap -> Point -> Int
basinSize heatmap = length . flip basinPoints []
    where
        xSize = length (head heatmap)
        ySize = length heatmap
        basinPoints :: Point -> [Point] -> [Point]
        basinPoints (x, y) visited =
            if 0 <= x && x < xSize && 0 <= y && y < ySize
                    && (heatmap !! y) !! x /= 9 && (x, y) `notElem` visited
                then basinPoints (x, y - 1)
                        $ basinPoints (x + 1, y)
                        $ basinPoints (x, y + 1)
                        $ basinPoints (x - 1, y) ((x, y):visited)
                else visited

task2 :: Heatmap -> [Point] -> Int
task2 heatmap = product . take 3 . reverse . sort . map (basinSize heatmap)


-- Main

main :: IO ()
main = do
    heatmap <- loadHeatmap "testinput.txt"
    heatmap <- loadHeatmap "input.txt"
    let lowPoints = findLowPoints heatmap
    print $ task1 heatmap lowPoints
    print $ task2 heatmap lowPoints
