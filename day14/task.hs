import Data.HashMap.Strict hiding (map, foldr)
import Data.Maybe

type Element = Char
type Polymer = HashMap (Element, Element) Int
type Rule = ((Element, Element), Element)
type Template = (Polymer, [Rule])


-- Input

loadTemplate :: String -> IO Template
loadTemplate = fmap (parseTemplate . lines) . readFile
    where
        parseTemplate :: [String] -> Template
        parseTemplate (polymer:_:rules) =
            (insert ('#', head polymer) 1 (parsePolymer polymer),
             map parseRule rules)
        parsePolymer :: String -> Polymer
        parsePolymer [a] = singleton (a, '#') 1
        parsePolymer (a:b:p) = insertWith (+) (a, b) 1 $ parsePolymer (b:p)
        parseRule :: String -> Rule
        parseRule rule = let [[a, b], _, [c]] = words rule in ((a, b), c)


-- Task

expandPolymer :: [Rule] -> Polymer -> Polymer
expandPolymer [] polymer = polymer
expandPolymer (((a, b), m):rules) polymer =
        let newPolymer = expandPolymer rules polymer
            pairCount = fromJust (polymer !? (a, b))
            in
                if isJust (polymer !? (a, b))
                    then insertWith (+) (a, m) pairCount
                        $ insertWith (+) (m, b) pairCount
                        $ update (\c -> if c <= pairCount
                                            then Nothing
                                            else Just (c - pairCount))
                          (a, b) newPolymer
                    else newPolymer

expandMultiple :: Int -> [Rule] -> Polymer -> Polymer
expandMultiple 0 _ = id
expandMultiple n rules = expandMultiple (n - 1) rules . expandPolymer rules

task :: Int -> Template -> Int
task n (startingPolymer, rules) = let
    expandedPolymer = expandMultiple n rules startingPolymer
    elementCounts = delete '#' $ foldrWithKey
        (\(a, b) c -> insertWith (+) a c . insertWith (+) b c) empty
        expandedPolymer
    counts = elems elementCounts
    in (maximum counts `div` 2) - (minimum counts `div` 2)


-- Task 1

task1 :: Template -> Int
task1 = task 10


-- Task 2

task2 :: Template -> Int
task2 = task 40

-- Main

main :: IO ()
main = do
    template <- loadTemplate "./input.txt"
    print $ task1 template
    print $ task2 template
