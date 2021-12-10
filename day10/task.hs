import Data.Functor
import Debug.Trace
import Data.List (sort)


type SyntaxResult = (String, Maybe Char)

-- Input

loadChunks :: String -> IO [String]
loadChunks file = readFile file <&> lines


-- Task

checkSyntax :: [String] -> [SyntaxResult]
checkSyntax = map (check "")
    where
        check :: String -> String -> SyntaxResult
        check chunks       []       = (chunks, Nothing)
        check chunks       ('(':xs) = check (')':chunks) xs
        check chunks       ('[':xs) = check (']':chunks) xs
        check chunks       ('{':xs) = check ('}':chunks) xs
        check chunks       ('<':xs) = check ('>':chunks) xs
        check (chunk:chunks) (x:xs) = if chunk == x
                                          then  check chunks xs
                                          else (chunk:chunks, Just x)
        -- check (')':chunks) (')':xs) = check chunks       xs
        -- check (']':chunks) (']':xs) = check chunks       xs
        -- check ('}':chunks) ('}':xs) = check chunks       xs
        -- check ('>':chunks) ('>':xs) = check chunks       xs
        -- check chunks       (x:xs)   = (chunks, Just x)


-- Task 1

corruptScore :: SyntaxResult -> Int
corruptScore (_, Nothing ) = 0
corruptScore (_, Just ')') = 3
corruptScore (_, Just ']') = 57
corruptScore (_, Just '}') = 1197
corruptScore (_, Just '>') = 25137

task1 :: [SyntaxResult] -> Int
task1 = sum . map corruptScore


-- Task 2


autocompleteScore :: String -> Int
autocompleteScore = autocomplete . reverse
    where
        autocomplete :: String -> Int
        autocomplete ""           = 0
        autocomplete (')':chunks) = 1 + 5 * autocomplete chunks
        autocomplete (']':chunks) = 2 + 5 * autocomplete chunks
        autocomplete ('}':chunks) = 3 + 5 * autocomplete chunks
        autocomplete ('>':chunks) = 4 + 5 * autocomplete chunks

task2 :: [SyntaxResult] -> Int
task2 syntaxResults = let
    scores = map (autocompleteScore . fst) $ filter ((== Nothing) . snd) syntaxResults
    in sort scores !! (length scores `div` 2)

-- Main
main :: IO ()
main = do
    input <- loadChunks "testinput.txt"
    input <- loadChunks "input.txt"
    let syntaxResults = checkSyntax input
    print $ task1 syntaxResults
    print $ task2 syntaxResults
