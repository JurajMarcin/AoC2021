import Debug.Trace
import Data.List (transpose)


-- Input

loadReport :: String -> IO [String]
loadReport file = readFile file >>= return . words


-- Task

fromBinary :: String -> Int
fromBinary = foldl (\a b -> (2 * a ) + (read [b] :: Int)) 0

invert :: String -> String
invert = map (\b -> if b == '1' then '0' else '1')


-- Task 1

gammaRate :: [String] -> String
gammaRate = map (\ bits ->
                if (length . filter (== '1')) bits >= length bits `div` 2
                    then '1'
                    else '0')


task1 :: [String] -> Int
task1 report = fromBinary gamma * fromBinary epsilon
    where
        gamma :: String
        gamma = gammaRate $ transpose report
        epsilon :: String
        epsilon = invert gamma


-- Task 2

type Criteria = [String] -> [String] -> ([String], Char)

mostCommon :: Criteria
mostCommon ones zeros = if length ones >= length zeros
                           then (ones, '1')
                           else (zeros, '0')

leastCommon :: Criteria
leastCommon ones zeros = if length zeros <= length ones
                            then (zeros, '0')
                            else (ones, '1')


rate :: Criteria -> [String] -> [String] -> [String] -> String
rate _    []   []    [""]                   = []
rate _    []   []    [bits]                 = bits
rate crit ones zeros []                     =
        let (lines, bit) = crit ones zeros
            in (bit : rate crit [] [] lines)
rate crit ones zeros ((bit : bits) : lines) =
        if bit == '0'
            then rate crit ones (bits : zeros) lines
            else rate crit (bits : ones) zeros lines

task2 :: [String] -> Int
task2 lines = fromBinary (rate mostCommon [] [] lines)
    * fromBinary (rate leastCommon [] [] lines)

-- Main

main :: IO ()
main = do
    report <- loadReport "input.txt"
    print $ task1 report
    print $ task2 report
