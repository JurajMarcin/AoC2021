import qualified Data.HashSet (map, foldr)
import Data.HashSet hiding (map, foldr)
import Debug.Trace

type Pos = (Int, Int)
type Paper = HashSet Pos

data Fold = AlongX Int
          | AlongY Int
          deriving (Show, Eq)

type Puzzle = (Paper, [Fold])


-- Input

loadPuzzle :: String -> IO Puzzle
loadPuzzle = fmap (parseDots . lines) . readFile
    where
        parseDots :: [String] -> Puzzle
        parseDots ("":ls) = (empty, parseFolds ls)
        parseDots (l:ls) = let
            (paper, folds) = parseDots ls
            [x, y] = map (read :: (String -> Int)) (split ',' l "")
            in (insert (x, y) paper, folds)
        parseFolds :: [String] -> [Fold]
        parseFolds [] = []
        parseFolds (l:ls) = let [ins, axis] = split '=' l "" in
            (if last ins == 'x'
                 then AlongX
                 else AlongY) (read axis :: Int) : parseFolds ls
        split :: Char -> String -> String -> [String]
        split _ [] s = [reverse s]
        split c (x:xs) s
            | c == x = reverse s : split c xs ""
            | otherwise = split c xs (x:s)


-- Task

foldPaper :: Fold -> Paper -> Paper
foldPaper fold = Data.HashSet.map (doFold fold)
    where
        doFold :: Fold -> Pos -> Pos
        doFold (AlongX fx) (x, y) =
            if x < fx then (x, y) else (2 * fx - x, y)
        doFold (AlongY fy) (x, y) =
            if y < fy then (x, y) else (x, 2 * fy - y)


-- Task 1

task1 :: Puzzle -> Int
task1 (paper, fold:_)= size $ foldPaper fold paper


-- Task 2

printPaper :: Paper -> IO ()
printPaper paper = printPaper' (0, 0) (paperSize paper) paper
    where
        paperSize :: Paper -> Pos
        paperSize paper = (maximum (map fst (toList paper)),
            maximum (map snd (toList paper)))
        printPaper' :: Pos -> Pos -> Paper -> IO ()
        printPaper' (x, y) (mx, my) paper
            | x > mx = putStrLn "" >> printPaper' (0, y + 1) (mx, my) paper
            | y > my = return ()
            | otherwise = let c = if member (x, y) paper then '#' else '.'
                in putChar c >> printPaper' (x + 1, y) (mx, my) paper

task2 :: Puzzle -> IO ()
task2 = printPaper . uncurry (foldl (flip foldPaper))


-- Main

main :: IO ()
main = do
    puzzle <- loadPuzzle "./testinput.txt"
    puzzle <- loadPuzzle "./input.txt"
    print $ task1 puzzle
    task2 puzzle
