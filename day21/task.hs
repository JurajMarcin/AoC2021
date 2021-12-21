{-# LANGUAGE TupleSections #-}
import Data.HashMap.Strict (HashMap, insert, member, (!?), empty)
import Data.Maybe (fromJust)

type Player = (Int, Int)


-- Input

loadPlayers :: String -> IO [Player]
loadPlayers = fmap (parsePlayers . map words . lines) . readFile
    where
        parsePlayers :: [[String]] -> [Player]
        parsePlayers = map ((, 0) . read . last)


-- Task 1

type Game = ([Player], Int)

rollDice :: (Int, Int) -> (Int, Int)
rollDice (y, x) = (y + x `mod` 100 + 1, x + 1)

step :: Game -> Game
step ((pos, score):players, dice) =
    let (roll, newDice) = foldr (const rollDice) (0, dice) [1 .. 3]
        newPos = ((pos - 1) + roll) `mod` 10 + 1
        newScore = score + newPos
    in  (players ++ [(newPos, newScore)], newDice)

play :: Game -> Game
play game@(players, _) =
    let hasWinner = any ((>= 1000) . snd) players
    in  if hasWinner then game else play $ step game

task1 :: [Player] -> Int
task1 players =
    let finishedGame = play (players, 0)
        (finalPlayers, finalRolls) = finishedGame
        othersScore = product $ filter (< 1000) $ map snd finalPlayers
    in  othersScore * finalRolls


-- Task 2

type Cache = HashMap [Player] [Int]

quantumStep :: [Player] -> Cache -> ([Int], Cache)
quantumStep ((pos, score):players) cache =
    let (wins, newCache) =
            foldr foldWins (repeat 0, cache)
                [players ++ [(newPos, newScore)]
                | r1 <- [1 .. 3], r2 <- [1 .. 3], r3 <- [1 .. 3],
                let newPos = (pos - 1 + r1 + r2 + r3) `mod` 10 + 1,
                let newScore = score + newPos]
    in  (wins, insert ((pos, score):players) wins newCache)
    where
        foldWins :: [Player] -> ([Int], Cache) -> ([Int], Cache)
        foldWins newPlayers (winsSoFar, cache) =
            let (newWins, newCache) = quantumPlay newPlayers cache
            in  (zipWith (+) winsSoFar (last newWins : init newWins),
                newCache)

quantumPlay :: [Player] -> Cache -> ([Int], Cache)
quantumPlay players cache =
    let winnerScores = map (min 1 . (`div` 21) . snd) players
        cachedResult = fromJust (cache !? players)
    in  if 1 `elem` winnerScores
            then (winnerScores, insert players winnerScores cache)
            else if member players cache
                     then (cachedResult, cache)
                     else quantumStep players cache

task2 :: [Player] -> Int
task2 = maximum . fst . flip quantumPlay empty


-- Main

main :: IO ()
main = do
    players <- loadPlayers "./input.txt"
    print $ task1 players
    print $ task2 players
