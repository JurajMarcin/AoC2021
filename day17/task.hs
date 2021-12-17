{-# LANGUAGE TupleSections #-}
import Data.Maybe
import Data.Bifunctor

data Reason = Overshoot | Hit | Undershoot
            deriving (Eq, Show)
type Pos = (Int, Int)
type Velocity = (Int, Int)
type Range = (Int, Int)
type TargetArea = (Range, Range)


-- Input

loadTargetArea :: String -> IO TargetArea
loadTargetArea = fmap (parseTargetArea . words) . readFile
    where
        parseTargetArea :: [String] -> TargetArea
        parseTargetArea [_, _, xr, yr] = (parseRange xr, parseRange yr)
        parseRange :: String -> (Int, Int)
        parseRange s =
            let d = drop 2 s -- <x|y>=<n1>..<n2>[,] -> <n1>..<n2>[,]
                a = takeWhile (/= '.') d -- <n1>..<n2>[,] -> <n1>
                b = takeWhile (/= ',') -- <n2>[,] -> <n2>
                    $ dropWhile (== '.') -- ..<n2>[,] -> <n2>[,]
                    $ dropWhile (/= '.') d -- <n1>..<n2>[,] -> ..<n2>[,]
            in (read a :: Int, read b :: Int)


-- Task

getTrajectory :: TargetArea -> Pos -> Velocity -> Maybe [Pos]
getTrajectory target@((x1, x2), (y1, y2)) (x, y) (vx, vy)
    | x > max x1 x2 = Nothing
    | y < min y1 y2 = Nothing
    | min x1 x2 <= x && x <= max x1 x2
        && min y1 y2 <= y && y <= max y1 y2 = Just [(x, y)]
    | otherwise =
        let nvx
                | vx > 0 = vx - 1
                | vx < 0 = vx + 1
                | otherwise = 0
            trajectory = getTrajectory target (x + vx, y + vy) (nvx, vy - 1)
        in if isNothing trajectory
               then Nothing
               else Just ((x, y) : fromJust trajectory)

getTrajectories :: TargetArea -> [(Velocity, [Pos])]
getTrajectories target@((x1, x2), (y1, y2)) = map (second fromJust)
    $ filter (isJust . snd)
        [((vx, vy), getTrajectory target (0, 0) (vx, vy))
        | vx <- [0 .. max x1 x2], vy <- [min y1 y2 .. (- min y1 y2)]]


-- Task 1

task1 :: [(Velocity, [Pos])] -> Int
task1 = maximum . map snd . concatMap snd


-- Task 1

task2 :: [(Velocity, [Pos])] -> Int
task2 = length


-- Main

main :: IO ()
main = do
    targetArea <- loadTargetArea "./input.txt"
    let trajectories = getTrajectories targetArea
    print $ task1 trajectories
    print $ task2 trajectories
