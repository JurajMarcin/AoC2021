import Debug.Trace

data SNumber = Empty | Regular Int | Pair SNumber SNumber
             deriving (Eq)

instance Show SNumber where
    show (Regular a) = show a
    show (Pair a b) = show [a, b]


-- Input

loadSNumbers :: String -> IO [SNumber]
loadSNumbers = fmap (map (fst . parseSNumber) . lines) . readFile

parseSNumber :: String -> (SNumber, String)
parseSNumber [] = (Empty, "")
parseSNumber ('[':s) =
    let (a, restA) = parseSNumber s
        (b, restB) = parseSNumber (drop 1 restA) -- drop ,
        rest = drop 1 restB -- drop ]
    in (Pair a b, rest)
parseSNumber s = let (n, rest) = break (\c -> c == ',' || c == ']') s
                 in  (Regular (read n :: Int), rest)


-- Task

reduce :: SNumber -> SNumber
reduce s = let (_, _, explodedS, exploded) = tryExplode 1 s
               (splitS, split) = trySplit explodedS
            in if exploded
                   then reduce explodedS
                   else if split then reduce splitS else splitS

tryExplode :: Int -> SNumber -> (Int, Int, SNumber, Bool)
tryExplode depth (Regular a) = (0, 0, Regular a, False)
tryExplode depth (Pair (Regular a) (Regular b)) =
    if depth > 4
        then (a, b, Regular 0, True)
        else (0, 0, Pair (Regular a) (Regular b), False)
tryExplode depth (Pair a b) =
    let (addLeftA, addRightA, newA, explodedA) =
            tryExplode (depth + 1) a
        (addLeftB, addRightB, newB, explodedB) =
            tryExplode (depth + 1) b
    in  if explodedA
            then (addLeftA, 0, Pair newA (addRight addRightA b),
                    True)
            else if explodedB
                     then (0, addRightB,
                          Pair (addLeft addLeftB a) newB, True)
                     else (0, 0, Pair a b, False)
    where
        addLeft :: Int -> SNumber -> SNumber
        addLeft n (Regular a) = Regular (a + n)
        addLeft n (Pair a b) = Pair a (addLeft n b)
        addRight :: Int -> SNumber -> SNumber
        addRight n (Regular a) = Regular (a + n)
        addRight n (Pair a b) = Pair (addRight n a) b

trySplit :: SNumber -> (SNumber, Bool)
trySplit (Regular a) =
    if a >= 10
        then (Pair (Regular (a `div` 2))
                (Regular (a `div` 2 + a `mod` 2)), True)
        else (Regular a, False)
trySplit (Pair a b) =
    let (newA, splitA) = trySplit a
        (newB, splitB) = trySplit b
    in  if splitA
            then (Pair newA b, splitA)
            else (Pair a newB, splitB)

instance Num SNumber where
    Empty + a = reduce a
    a + Empty = reduce a
    a + b = reduce (Pair a b)
    fromInteger 0 = Empty
    (-) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    negate = undefined

getMagnitude :: SNumber -> Int
getMagnitude (Regular a) = a
getMagnitude (Pair a b) = getMagnitude a * 3 + getMagnitude b * 2


-- Task 1

task1 :: [SNumber] -> Int
task1 = getMagnitude . sum


-- Task 2

task2 :: [SNumber] -> Int
task2 sNumbers = maximum $ concat
    [ [getMagnitude (a + b), getMagnitude (b + a)]
    | a <- sNumbers, b <- sNumbers, a /= b ]


-- Tests

test :: IO ()
test = do
    putStrLn "Parse tests"
    let cases = [("[1,2]", "([1,2],\"\")")
                ,("[[1,2],3]", "([[1,2],3],\"\")")
                ,("[9,[8,7]]", "([9,[8,7]],\"\")")
                ,("[[1,9],[8,5]]", "([[1,9],[8,5]],\"\")")
                ,("[[[[1,2],[3,4]],[[5,6],[7,8]]],9]",
                  "([[[[1,2],[3,4]],[[5,6],[7,8]]],9],\"\")")
                ,("[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]",
                  "([[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]],\"\")")
                ,("[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],"
                  ++ "[7,3]]]]",
                  "([[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],"
                  ++ "[7,3]]]],\"\")")]
    print $ map (\(i, o) -> show (parseSNumber i) == o) cases
    putStrLn "Explode tests"
    let cases = [("[[[[[9,8],1],2],3],4]", "(9,0,[[[[0,9],2],3],4],True)")
                ,("[7,[6,[5,[4,[3,2]]]]]", "(0,2,[7,[6,[5,[7,0]]]],True)")
                ,("[[6,[5,[4,[3,2]]]],1]", "(0,0,[[6,[5,[7,0]]],3],True)")
                ,("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]",
                  "(0,0,[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]],True)")
                ,("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
                  "(0,2,[[3,[2,[8,0]]],[9,[5,[7,0]]]],True)")]
    print $ map (\(i, o) -> show
        (tryExplode 1 (fst (parseSNumber i))) == o) cases
    putStrLn "Split tests"
    let cases = [("[[[[0,7],4],[15,[0,13]]],[1,1]]",
                  "([[[[0,7],4],[[7,8],[0,13]]],[1,1]],True)")
                ,("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]",
                  "([[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]],True)")]
    print $ map (\(i, o) -> show (trySplit (fst (parseSNumber i))) == o)
        cases
    putStrLn "Reduce test"
    print $ show (reduce (fst
        (parseSNumber "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")))
        == "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"


-- Main

main :: IO ()
main = do
    sNumbers <- loadSNumbers "./input.txt"
    print $ task1 sNumbers
    print $ task2 sNumbers
