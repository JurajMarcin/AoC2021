data Command = Forward Int
             | Down Int
             | Up Int

type Position = (Int, Int, Int)


-- Input

parseCommands :: [String] -> [Command]
parseCommands [] = []
parseCommands ("forward" : x : rest) =
    Forward (read x :: Int) : parseCommands rest
parseCommands ("down" : x : rest) =
    Down (read x :: Int) : parseCommands rest
parseCommands ("up" : x : rest) =
    Up (read x :: Int) : parseCommands rest

loadCourse :: String -> IO [Command]
loadCourse file = readFile file >>= return . parseCommands . words


-- Task 1

task1Interpreter :: Command -> Position -> Position
task1Interpreter (Forward x) (hp, depth, _) = (hp + x, depth, 0)
task1Interpreter (Down    x) (hp, depth, _) = (hp, depth + x, 0)
task1Interpreter (Up      x) (hp, depth, _) = (hp, depth - x, 0)


-- Task 2

task2Interpreter :: Command -> Position -> Position
task2Interpreter (Forward x) (hp, depth, aim) =
    (hp + x, depth + aim * x, aim)
task2Interpreter (Down    x) (hp, depth, aim) = (hp, depth, aim + x)
task2Interpreter (Up      x) (hp, depth, aim) = (hp, depth, aim - x)


-- Task

task :: (Command -> Position -> Position) -> [Command] -> Int
task interpreter = flip navigator (0, 0, 0)
    where
        navigator :: [Command] -> Position -> Int
        navigator [] (hp, depth, _) = hp * depth
        navigator (cmd:cmds) pos = (navigator cmds . interpreter cmd) pos


-- Main

main :: IO ()
main = do
    commands <- loadCourse "input.txt"
    print $ task task1Interpreter commands
    print $ task task2Interpreter commands
