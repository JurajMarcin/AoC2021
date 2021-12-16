import Data.HashMap.Strict
import qualified Data.Heap as Heap
import Data.Maybe (fromJust, isNothing, isJust)
import Debug.Trace

type Pos = (Int, Int)
type RiskMap = HashMap Pos Int


-- Input

loadRiskMap :: String -> IO RiskMap
loadRiskMap = fmap (parseMap (0, 0) . lines) . readFile
    where
        parseMap :: Pos -> [String] -> RiskMap
        parseMap (x, y) [] = empty
        parseMap (x, y) ([]:lines) = parseMap (0, y + 1) lines
        parseMap (x, y) ((r:line):lines) = insert (x, y) (read [r] :: Int)
            $ parseMap (x + 1, y) (line:lines)


-- Task

type RiskHeap = Heap.Heap (Heap.Entry Int Pos)

getMinRisk :: RiskMap -> Pos -> Int
getMinRisk riskMap end =
        dijkstra empty $ Heap.singleton $ Heap.Entry 0 (0, 0)
    where
        dijkstra :: RiskMap -> RiskHeap -> Int
        dijkstra minRiskMap heap =
            let (Heap.Entry minRisk (x, y), newHeap) =
                    fromJust $ Heap.uncons heap
                visited = isJust $ minRiskMap !? (x, y)
                up = (x, y - 1)
                down = (x, y + 1)
                left = (x - 1, y)
                right = (x + 1, y)
            in  if (x, y) == end
                    then minRisk
                    else
                        if visited
                            then dijkstra minRiskMap newHeap
                            else dijkstra
                                    (insert (x, y) minRisk minRiskMap)
                                    $ insertMaybe minRisk up
                                        (riskMap !? up)
                                    $ insertMaybe minRisk down
                                        (riskMap !? down)
                                    $ insertMaybe minRisk left
                                        (riskMap !? left)
                                    $ insertMaybe minRisk right
                                        (riskMap !? right) newHeap
        insertMaybe :: Int -> Pos -> Maybe Int -> RiskHeap -> RiskHeap
        insertMaybe _  _ Nothing = id
        insertMaybe minRisk pos (Just risk) =
            Heap.insert (Heap.Entry (minRisk + risk) pos)


-- Task 1

task1 :: RiskMap -> Int
task1 riskMap = let x = floor $ sqrt $ fromIntegral $ size riskMap
                    in getMinRisk riskMap (x - 1, x - 1)


-- Task 2

enlargeRiskMap :: Int -> RiskMap -> RiskMap
enlargeRiskMap n riskMap = unions [newRiskMap ra (xa, ya)
                                  | xa <- [0 .. (n - 1)],
                                    ya <- [0 .. (n - 1)],
                                    let ra = xa + ya]
    where
        s = floor $ sqrt $ fromIntegral $ size riskMap
        newRiskMap :: Int -> Pos -> RiskMap
        newRiskMap ra (xa, ya) =
            mapKeys (\(x, y) -> (x + xa * s, y + ya * s))
            $ Data.HashMap.Strict.map (\r -> if r + ra > 9 then r + ra - 9 else r + ra) riskMap

task2 :: RiskMap -> Int
task2 = task1 . enlargeRiskMap 5


-- Main

main :: IO ()
main = do
    riskMap <- loadRiskMap "./testinput.txt"
    riskMap <- loadRiskMap "./input.txt"
    print $ task1 riskMap
    print $ task2 riskMap
