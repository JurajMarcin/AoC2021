import Data.List
import Data.Functor

type LanternPack = (Int, Int)


-- Input

parseAges :: String -> [LanternPack]
parseAges = grouper (-1, 0) . sort . parser ""
    where
        grouper :: LanternPack -> [Int] -> [LanternPack]
        grouper (-1, _)      (x:xs) = grouper (x, 1) xs
        grouper (age, count) []     = [(age, count)]
        grouper (age, count) (x:xs) =
            if x == age
                then grouper (age, count + 1) xs
                else (age, count) : grouper (x, 1) xs
        parser :: String -> String -> [Int]
        parser "" []     = []
        parser n  []     = [read (reverse n) :: Int]
        parser n  (x:xs) = if x == ','
                              then (read (reverse n) :: Int) : parser "" xs
                              else parser (x : n) xs

loadPacks :: String -> IO [LanternPack]
loadPacks file = readFile file <&> parseAges


-- Task


task :: Int -> [LanternPack] -> [LanternPack]
task 0 xs = xs
task n xs = task (n - 1) (day 0 0 xs)
    where
        day :: Int -> Int -> [LanternPack] -> [LanternPack]
        day 0  0  [] = []
        day 0  c8 [] = [(8, c8)]
        day c6 c8 [] = (6, c6) : day 0 c8 []
        day c6 c8 ((0, count) : xs) = day (c6 + count) (c8 + count) xs
        day c6 c8 ((age, count):xs) =
            if age - 1 == 6
                then (age - 1, count + c6) : day 0 c8 xs
                else (age - 1, count) : day c6 c8 xs


-- Main

main :: IO ()
main = do
    packs <- loadPacks "input.txt"
    print $ sum $ map snd $ task 80 packs
    print $ sum $ map snd $ task 256 packs
