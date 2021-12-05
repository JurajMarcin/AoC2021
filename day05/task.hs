import Data.List

type Coords = (Int, Int)
type Vent = (Coords, Coords)

sortCoords :: [Coords] -> [Coords]
sortCoords = sortBy (\(x1, y1) (x2, y2) -> if y1 == y2
                                               then compare x1 x2
                                               else compare y1 y2)

-- Input

loadVents :: String -> IO [Vent]
loadVents file = readFile file >>= return . parseVents . words
    where
        parseVents :: [String] -> [Vent]
        parseVents [] = []
        parseVents (c1:_:c2:rest) =
            (parseCoords c1 "", parseCoords c2 "") : parseVents rest
        parseCoords :: String -> String -> Coords
        parseCoords [] y = (0, read (reverse y) :: Int)
        parseCoords (',':as) x = let (_, y) = parseCoords as ""
                                     in (read (reverse x) :: Int, y)
        parseCoords (a:as) b = parseCoords as (a : b)


-- Task

ventsCoords :: [Vent] -> [Coords]
ventsCoords [] = []
ventsCoords vents = concatMap (uncurry  ventCoords) vents
    where
        ventCoords :: Coords -> Coords -> [Coords]
        ventCoords c1@(x1, y1) c2@(x2, y2) =
            if c1 == c2
                then [c1]
                else zip (range x1 x2) (range y1 y2)
        range :: Int -> Int -> [Int]
        range a b = if a == b
                        then repeat a
                        else [a, a + (if a < b then 1 else (-1)) .. b]

task :: [Coords] -> Int
task coords = countCoords (-1, -1) 0 (sortCoords coords)
    where
        countCoords :: Coords -> Int -> [Coords] -> Int
        countCoords _ x [] = if x >= 2 then 1 else 0
        countCoords c1 x (c2:cs) =
            if c1 == c2
                then countCoords c2 (x + 1) cs
                else (if x >= 2 then 1 else 0) + countCoords c2 1 cs


-- Main

main :: IO ()
main = do
    vents <- loadVents "input.txt"
    let hvVents =
            filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) vents
    print $ task $ ventsCoords hvVents
    print $ task $ ventsCoords vents
