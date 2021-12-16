type Version = Int
type Type = Int
data Operator = Sum | Product | Min | Max | Gt | Lt | Eq
              deriving (Eq, Show)
data Packet = ValuePacket Version Int
            | OperatorPacket Version Operator [Packet]
            deriving (Eq, Show)


-- Input

fromBinaryR :: String -> Int
fromBinaryR = foldr (\c acc -> acc * 2 + (read [c] :: Int)) 0

fromBinary :: String -> Int
fromBinary = fromBinaryR . reverse

parseOperator :: String -> Operator
parseOperator "000" = Sum
parseOperator "001" = Product
parseOperator "010" = Min
parseOperator "011" = Max
parseOperator "101" = Gt
parseOperator "110" = Lt
parseOperator "111" = Eq

parsePacket :: String -> (Packet, String)
-- literal value
parsePacket (v1:v2:v3:'1':'0':'0':payload) =
    let (parsedValue, rest) = parseValue payload ""
    in (ValuePacket (fromBinaryR [v3, v2, v1]) parsedValue, rest)
    where
        parseValue :: String -> String -> (Int, String)
        parseValue ('1':a1:a2:a3:a4:payload) value =
            parseValue payload (a4:a3:a2:a1:value)
        parseValue ('0':a1:a2:a3:a4:payload) value =
            (fromBinaryR (a4:a3:a2:a1:value), payload)
-- operator (total length)
parsePacket (v1:v2:v3:o1:o2:o3:'0':payload) =
    let totalLength = fromBinary $ take 15 payload
        (parsedSubpackets, _) =
            parseSubpackets (take totalLength (drop 15 payload))
    in (OperatorPacket (fromBinaryR [v3, v2, v1])
            (parseOperator [o1, o2, o3]) parsedSubpackets,
        drop (totalLength + 15) payload)
    where
        parseSubpackets :: String -> ([Packet], String)
        parseSubpackets payload =
            if length payload < 6
                then ([], payload)
                else let
                        (packet, newPayload) = parsePacket payload
                        (packets, rest) = parseSubpackets newPayload
                    in (packet : packets, rest)
-- operator (count)
parsePacket (v1:v2:v3:o1:o2:o3:'1':payload) =
    let count = fromBinary $ take 11 payload
        (parsedSubpackets, rest) = parseSubpackets count (drop 11 payload)
    in
        (OperatorPacket (fromBinaryR [v3, v2, v1]) 
            (parseOperator [o1, o2, o3]) parsedSubpackets, rest)
    where
        parseSubpackets :: Int -> String -> ([Packet], String)
        parseSubpackets 0 payload = ([], payload)
        parseSubpackets count payload =
            let (packet, newPayload) = parsePacket payload
                (packets, rest) = parseSubpackets (count - 1) newPayload
            in (packet : packets, rest)

loadPacket :: String -> IO Packet
loadPacket = fmap (fst . parsePacket . hex2bin) . readFile
    where
        hex2bin :: String -> String
        hex2bin [] = []
        hex2bin ('0':s) = "0000" ++ hex2bin s
        hex2bin ('1':s) = "0001" ++ hex2bin s
        hex2bin ('2':s) = "0010" ++ hex2bin s
        hex2bin ('3':s) = "0011" ++ hex2bin s
        hex2bin ('4':s) = "0100" ++ hex2bin s
        hex2bin ('5':s) = "0101" ++ hex2bin s
        hex2bin ('6':s) = "0110" ++ hex2bin s
        hex2bin ('7':s) = "0111" ++ hex2bin s
        hex2bin ('8':s) = "1000" ++ hex2bin s
        hex2bin ('9':s) = "1001" ++ hex2bin s
        hex2bin ('A':s) = "1010" ++ hex2bin s
        hex2bin ('B':s) = "1011" ++ hex2bin s
        hex2bin ('C':s) = "1100" ++ hex2bin s
        hex2bin ('D':s) = "1101" ++ hex2bin s
        hex2bin ('E':s) = "1110" ++ hex2bin s
        hex2bin ('F':s) = "1111" ++ hex2bin s


-- Task 1

task1 :: Packet -> Int
task1 (ValuePacket version _) = version
task1 (OperatorPacket version _ packets) =
    version + sum (map task1 packets)


-- Task 2

eval :: Packet -> Int
eval (ValuePacket _ value) = value
eval (OperatorPacket _ Sum packets) = sum $ map eval packets
eval (OperatorPacket _ Product packets) = product $ map eval packets
eval (OperatorPacket _ Min packets) = minimum $ map eval packets
eval (OperatorPacket _ Max packets) = maximum $ map eval packets
eval (OperatorPacket _ Gt [packet1, packet2]) =
    if eval packet1 > eval packet2 then 1 else 0
eval (OperatorPacket _ Lt [packet1, packet2]) =
    if eval packet1 < eval packet2 then 1 else 0
eval (OperatorPacket _ Eq [packet1, packet2]) =
    if eval packet1 == eval packet2 then 1 else 0

task2 :: Packet -> Int
task2 = eval


-- Main

main :: IO ()
main = do
    packet <- loadPacket "./testinput4.txt"
    packet <- loadPacket "./input.txt"
    print $ task1 packet
    print $ task2 packet
