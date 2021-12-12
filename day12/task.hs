import Data.HashMap.Strict (HashMap, insertWith, empty, (!?), insert)
import Data.Char
import Data.Functor
import Data.Hashable
import Data.Maybe (fromJust, isNothing)


data Cave = LargeCave String
          | SmallCave String
          deriving (Eq, Show)
instance Hashable Cave where
        hashWithSalt s (LargeCave c) = hashWithSalt s c
        hashWithSalt s (SmallCave c) = hashWithSalt s c

type CaveSystem = HashMap Cave [Cave]
type VisitedMap = HashMap Cave Bool


-- Input

loadCaveSystem :: String -> IO CaveSystem
loadCaveSystem file = readFile file <&> parseCaves . lines
    where
        parseCaves :: [String] -> CaveSystem
        parseCaves [] = empty
        parseCaves (x:xs) = let [c1, c2] = map parseCave (split x "") in
            insertWith (++) c1 [c2]
            $ insertWith (++) c2 [c1]
            $ parseCaves xs
        split :: String -> String -> [String]
        split [] s = [reverse s]
        split ('-':xs) s = reverse s : split xs ""
        split (x:xs) s = split xs (x:s)
        parseCave :: String -> Cave
        parseCave s
            | all isLower s = SmallCave s
            | all isUpper s = LargeCave s


-- Task

type Path = [Cave]

findPaths :: Cave -> CaveSystem -> VisitedMap -> Bool -> [Path]
findPaths (SmallCave "end") system visited _ = [[SmallCave "end"]]
findPaths start system visited smallTwice =
        if canVisit
            then fst $ foldr traverse ([], insert start True visited)
                    (fromJust (system !? start))
            else []
    where
        visitedStart = (visited !? start) == Just True
        canVisit = case start of
                    (SmallCave "start") -> not visitedStart
                    (SmallCave _) -> not visitedStart || smallTwice
                    (LargeCave _) -> True
        nextSmallTwice = smallTwice && (case start of
                                        (SmallCave _) -> not visitedStart
                                        (LargeCave _) -> True)
        traverse :: Cave -> ([Path], VisitedMap) -> ([Path], VisitedMap)
        traverse neigh (paths, visited) =
            let newPaths = findPaths neigh system visited nextSmallTwice in
                (map (start :) newPaths ++ paths, visited)


-- Task 1

task1 :: CaveSystem -> Int
task1 = length . flip (flip (findPaths (SmallCave "start")) empty) False


-- Task 2

task2 :: CaveSystem -> Int
task2 = length . flip (flip (findPaths (SmallCave "start")) empty) True


-- Main

main :: IO ()
main = do
    caveSystem <- loadCaveSystem "input.txt"
    print $ task1 caveSystem
    print $ task2 caveSystem
