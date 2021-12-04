{-# LANGUAGE TupleSections #-}
import System.IO
import Data.List

type Board = [[(Int, Bool)]]

-- Input

split :: String -> Char -> [Int]
split str c = map (read :: (String -> Int))
        $ filter (not . null) $ innerSplit str ""
    where
        innerSplit :: String -> String -> [String]
        innerSplit []     y = [reverse y]
        innerSplit (x:xs) y = if x == c
                                  then reverse y : innerSplit xs ""
                                  else innerSplit xs (x : y)

loadNumbers :: IO [Int]
loadNumbers = getLine >>= \line -> return $ split line ','

loadBoard :: IO Board
loadBoard = loadBoardRow 0
        where
            loadBoardRow :: Int -> IO Board
            loadBoardRow 5 = return []
            loadBoardRow x = do
                line <- getLine
                let row = map (, False) $ split line ' '
                rows <- loadBoardRow (x + 1)
                return $ row : rows

loadBoards :: IO [Board]
loadBoards = isEOF >>= \eof -> if eof
                                   then return []
                                   else do
                                       getLine
                                       board <- loadBoard
                                       boards <- loadBoards
                                       return $ board : boards


-- Tasks

isWinningBoard :: Board -> Bool
isWinningBoard board = hasWinningRow board
        || hasWinningRow (transpose board)
    where
        hasWinningRow :: Board -> Bool
        hasWinningRow = any (all snd)

selectInBoard :: Int -> Board -> Board
selectInBoard num = map (map (\(n, s) -> (n, s || n == num)))

getWinningBoards :: [Board] -> [Int] -> [(Board, Int)]
getWinningBoards _      []           = []
getWinningBoards boards (num : nums) =
    map (, num) wBoards ++ getWinningBoards cBoards nums
        where
            sBoards = map (selectInBoard num) boards
            wBoards = filter isWinningBoard sBoards
            cBoards = filter (not . isWinningBoard) sBoards

score :: (Board, Int) -> Int
score (board, num) = num * sum (map fst $ filter (not . snd) $ concat board)


main :: IO ()
main = do
        numbers <- loadNumbers
        boards <- loadBoards
        let winningBoards = getWinningBoards boards numbers
        print $ score $ head winningBoards
        print $ score $ last winningBoards
