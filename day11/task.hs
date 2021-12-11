import Data.HashMap.Strict
import Data.Functor
import Data.Maybe (isNothing, fromJust)

type Pos = (Int, Int)
type Energy = Int
type Octomap = HashMap Pos Energy
type Flashmap = HashMap Pos Bool


-- Input

loadOctomap :: String -> IO Octomap
loadOctomap file = readFile file <&> parseLines emptyOctomap 0 . lines
    where
        emptyOctomap :: Octomap
        emptyOctomap = empty
        parseLines :: Octomap -> Int -> [String] -> Octomap
        parseLines octomap _ []     = octomap
        parseLines octomap y (l:ls) = parseLines (parseLine octomap 0 y l) (y + 1) ls
        parseLine :: Octomap -> Int -> Int -> String -> Octomap
        parseLine octomap  _ _ []     = octomap
        parseLine octomap x y (c:cs) = parseLine (insert (x, y) (read [c] :: Int) octomap) (x + 1) y cs


-- Task

increaseEnergy :: Pos -> (Octomap, Flashmap) -> (Octomap, Flashmap)
increaseEnergy (x, y) (octomap, flashmap) =
        if isNothing (flashmap !? (x, y))
            then let newOctomap = adjust (+ 1) (x, y) octomap in
                 if newOctomap !? (x, y) == Just 10
                    then increaseEnergy (x, y - 1)
                         $ increaseEnergy (x + 1, y - 1)
                         $ increaseEnergy (x + 1, y)
                         $ increaseEnergy (x + 1, y + 1)
                         $ increaseEnergy (x, y + 1)
                         $ increaseEnergy (x - 1, y + 1)
                         $ increaseEnergy (x - 1, y)
                         $ increaseEnergy (x - 1, y - 1)
                            (insert (x, y) 0 newOctomap,
                             insert (x, y) True flashmap)
                     else (newOctomap, flashmap)
            else (octomap, flashmap)

step :: Octomap -> (Octomap, Int)
step octomap = (newOctomap, size flashmap)
    where
        (newOctomap, flashmap) = foldrWithKey increase (octomap, emptyFlashmap) octomap
        emptyFlashmap :: Flashmap
        emptyFlashmap = empty
        increase :: Pos -> Int -> (Octomap, Flashmap) -> (Octomap, Flashmap)
        increase pos _ maps = increaseEnergy pos maps

steps :: Int -> Octomap -> (Octomap, Int)
steps 0 octomap = (octomap, 0)
steps n octomap = let (restOctomap, restFlashes) = steps (n - 1) octomap
                      (newOctomap, flashes) = step restOctomap
                      in (newOctomap, restFlashes + flashes)


-- Task 1

task1 :: Octomap -> Int
task1 = snd . steps 100


-- Task 2

task2 :: Octomap -> Int
task2 = findSync 1
    where
        findSync :: Int -> Octomap -> Int
        findSync n octomap = let (newOctomap, flashes) = step octomap in
            if size newOctomap == flashes
                then n
                else findSync (n + 1) newOctomap


-- Main

main :: IO ()
main = do
    octomap <- loadOctomap "input.txt"
    print $ task1 octomap
    print $ task2 octomap
