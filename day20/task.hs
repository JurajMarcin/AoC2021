import Data.HashSet hiding (map)
import Data.List (sort)


type Pos = (Int, Int)
type Bounds = (Pos, Pos)
type IEA = HashSet Int
type ImagePart = HashSet Pos
type Image = (ImagePart, Bool)


-- Input

loadInput :: String -> IO (IEA, Image)
loadInput = fmap (parseInput . lines) . readFile
    where
        parseInput :: [String] -> (IEA, Image)
        parseInput (iea:_:image) =
            (parseIea 0 iea, (parseImage (0, 0) image, False))
        parseIea :: Int -> String -> IEA
        parseIea _ [] = empty
        parseIea i ('.':cs) = parseIea (i + 1) cs
        parseIea i ('#':cs) = insert i $ parseIea (i + 1) cs
        parseImage :: Pos -> [String] -> ImagePart
        parseImage _ [] = empty
        parseImage (_, y) ([]:ls) = parseImage (0, y + 1) ls
        parseImage (x, y) (('.':cs):ls) = parseImage (x + 1, y) (cs:ls)
        parseImage (x, y) (('#':cs):ls) =
            insert (x, y) $ parseImage (x + 1, y) (cs:ls)


-- Task

isInBounds :: Pos -> Bounds -> Bool
isInBounds (x, y) ((x1, y1), (x2, y2)) =
    (x1 <= x && x <= x2) && (y1 <= y && y <= y2)

getPixel :: Pos -> Image -> Bounds -> Int
getPixel pos image@(imagePart, outside) bounds =
    if member pos imagePart || (not (isInBounds pos bounds) && outside)
        then 1
        else 0

calculateIeaIndex :: Pos -> Image -> Bounds -> Int
calculateIeaIndex (x, y) image bounds =
    getPixel (x - 1, y - 1) image bounds * 2 ^ 8
    + getPixel (x    , y - 1) image bounds * 2 ^ 7
    + getPixel (x + 1, y - 1) image bounds * 2 ^ 6
    + getPixel (x - 1, y    ) image bounds * 2 ^ 5
    + getPixel (x    , y    ) image bounds * 2 ^ 4
    + getPixel (x + 1, y    ) image bounds * 2 ^ 3
    + getPixel (x - 1, y + 1) image bounds * 2 ^ 2
    + getPixel (x    , y + 1) image bounds * 2 ^ 1
    + getPixel (x + 1, y + 1) image bounds

enhancePixel :: Pos -> Image -> Bounds -> IEA -> Bool
enhancePixel pos image = member . calculateIeaIndex pos image

getImageBounds :: Image -> Bounds
getImageBounds (imagePart, _) =
    let points = toList imagePart
        x = map fst points
        y = map snd points
    in ((minimum x, minimum y), (maximum x, maximum y))

enhance :: IEA -> Image -> Image
enhance iea image =
    let bounds@((x1, y1), (x2, y2)) = getImageBounds image
    in  (fromList [ (x, y) | x <- [x1 - 1 .. x2 + 1],
                             y <- [y1 - 1 .. y2 + 1],
                             enhancePixel (x, y) image bounds iea ],
         enhancePixel (x1 - 2, y1 - 2) image bounds iea)

multiEnhance :: Int -> IEA -> Image -> Image
multiEnhance 0 _ = id
multiEnhance n iea = enhance iea . multiEnhance (n - 1) iea


-- Task 1

task1 :: IEA -> Image -> Int
task1 iea = size . fst . multiEnhance 2 iea


-- Task 2

task2 :: IEA -> Image -> Int
task2 iea = size . fst . multiEnhance 50 iea


-- Main

main :: IO ()
main = do
    (iea, image) <- loadInput "./input.txt"
    print $ task1 iea image
    print $ task2 iea image
