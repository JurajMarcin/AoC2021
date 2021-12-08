import Data.Functor
import Debug.Trace
import Data.List
import Data.List.Extra (notNull)

type Signal = String
type Entry = ([Signal], [Signal])


-- Input

loadEntries :: String -> IO [Entry]
loadEntries file = readFile file <&> map (parseEntry . words) . lines
    where
        parseEntry :: [String] -> Entry
        parseEntry ("|":xs) = ([], xs)
        parseEntry (x:xs) = let (i, o) = parseEntry xs in (x : i, o)


-- Task 1

task1 :: [Entry] -> Int
task1 = length . filter is1478 . concatMap snd
    where
        is1478 :: String -> Bool
        is1478 o = case length o of
                       2 -> True  -- 1
                       4 -> True  -- 4
                       3 -> True  -- 7
                       7 -> True  -- 8
                       _ -> False


-- Task 2

type Segment = [Char]
type SegmentMap = [Segment]

data State = On | Off | Undec
           deriving (Eq, Show)

eliminate :: [Signal] -> SegmentMap -> SegmentMap
eliminate []     = deduplicate
    where
        deduplicate :: SegmentMap -> SegmentMap
        deduplicate s = let used = concat (filter ((== 1) . length) s) in
            map (\x -> if length x == 1 then x else x \\ used) s
eliminate (signal:signals) = eliminate signals . (case length signal of
          2 -> elim signal [Off, Off, On, Off, Off, On, Off]
          3 -> elim signal [On, Off, On, Off, Off, On, Off]
          4 -> elim signal [Off, On, On, On, Off, On, Off]
          5 -> elim signal [On, Undec, Undec, On, Undec, Undec, On]
          6 -> elim signal [On, On, Undec, Undec, Undec, On, On]
          7 -> elim signal [On, On, On, On, On, On, On])
    where
        elim :: Signal -> [State] -> SegmentMap -> SegmentMap
        elim signal = zipWith (f signal)
        f :: Signal -> State -> Segment -> Segment
        f signal On = filter (`elem` signal)
        f signal Off = filter (`notElem` signal)
        f signal Undec = id

sElem :: SegmentMap -> [Int] -> Signal -> Bool
sElem _ []     _     = True
sElem s (x:xs) input =
    (head (s !! x) `elem` input) && sElem s xs input

decode :: SegmentMap -> Signal -> Int
decode segMap signal
    | sElem segMap [0, 1, 2, 3, 4, 5, 6] signal = 8
    | sElem segMap [0, 1, 2, 4, 5, 6] signal = 0
    | sElem segMap [0, 1, 3, 4, 5, 6] signal = 6
    | sElem segMap [0, 1, 2, 3, 5, 6] signal = 9
    | sElem segMap [0, 2, 3, 4, 6] signal = 2
    | sElem segMap [0, 2, 3, 5, 6] signal = 3
    | sElem segMap [0, 1, 3, 5, 6] signal = 5
    | sElem segMap [1, 2, 3, 5] signal = 4
    | sElem segMap [0, 2, 5] signal = 7
    | sElem segMap [2, 5] signal = 1

task2 :: [Entry] -> Int
task2 = sum . map fn
    where
        initSegMap = replicate 7 "abcdefg"
        fn :: Entry -> Int
        fn (signals, output) =
            let segment = eliminate signals initSegMap in
                foldl (\r x -> r * 10 + x) 0 (map (decode segment) output)


-- Main

main :: IO ()
main = do
    entries <- loadEntries "testinput.txt"
    entries <- loadEntries "input.txt"
    print $ task1 entries
    print $ task2 entries
