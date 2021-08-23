import Data.Text ( pack, unpack, splitOn  )
import Data.List ( sort )

splitToCleanStringArr :: String -> [String]
splitToCleanStringArr str = do
    let arr = splitOn (pack " ") (pack str)
    Prelude.map unpack arr

checkForInvalidParams :: [String] -> Int -> Bool
checkForInvalidParams cmd arrLength  
    | cmd !! 0 == "reverse" ||
        cmd !! 0 == "sort" = do
            let start = read (cmd !! 2) :: Int -- 2
            let count = read (cmd !! 4) :: Int -- 4

            if  start < 0 || 
                start + count >= arrLength
            then False
            else True

    | cmd !! 0 == "rollLeft" ||
        cmd !! 0 == "rollRight" = do
            let count = read (cmd !! 1) :: Int

            if  count < 0
            then False
            else True

    | otherwise   = False

-- COMMANDS!!!

reverseCmd :: [String] -> [String] -> [String]
reverseCmd arr cmd = do
    let start = read (cmd !! 2) :: Int
    let count = read (cmd !! 4) :: Int

    let firstSplit = splitAt start arr -- fst this -> don't touch till end
    let secondSplit = splitAt count (snd firstSplit) -- snd -> don't touch till end

    let idk = reverse (fst secondSplit)
    fst firstSplit ++ idk ++ snd secondSplit

sortCmd :: [String] -> [String] -> [String]
sortCmd arr cmd = do
    let start = read (cmd !! 2) :: Int
    let count = read (cmd !! 4) :: Int

    let firstSplit = splitAt start arr -- fst this -> don't touch till end
    let secondSplit = splitAt count (snd firstSplit) -- snd -> don't touch till end

    let idk = sort (fst secondSplit)
    fst firstSplit ++ idk ++ snd secondSplit

rotateLeft :: [String] -> Int -> [String]
rotateLeft x n = take (length x) $ drop (length x + n) $ cycle x

rotateRight :: [String] -> Int -> [String]
rotateRight x n = do
    let y = reverse x
    let rotated = take (length y) $ drop (length y + n) $ cycle y
    reverse rotated

rollLeftCmd :: [String] -> [String] -> [String]
rollLeftCmd arr cmd = do
    let count = read (cmd !! 1) :: Int
    rotateLeft arr count

rollRightCmd :: [String] -> [String] -> [String]
rollRightCmd arr cmd = do
    let count = read (cmd !! 1) :: Int
    rotateRight arr count

-- End of COMMANDS!!!

performOperationsOnArr :: [String] -> [String] -> [String] 
performOperationsOnArr arr cmd = 
    case cmd !! 0 of
        "reverse"   -> reverseCmd arr cmd
        "sort"      -> sortCmd arr cmd
        "rollLeft"  -> rollLeftCmd arr cmd
        "rollRight" -> rollRightCmd arr cmd
        _           -> ["????"]

readUntilEnd :: [String] -> IO ()
readUntilEnd arr = do
    command <- getLine
    if command == "end"
    then print arr
    else do
        let cmd = splitToCleanStringArr command
        if not (checkForInvalidParams cmd (length arr))
        then do
            print "Invalid input parameters"

            print arr
            print cmd
            print $ length arr

            readUntilEnd arr
        else do
            let result = performOperationsOnArr arr cmd
            readUntilEnd result

commandInterpreter :: IO()
commandInterpreter = do
    line <- getLine
    let cleanArr = splitToCleanStringArr line
    readUntilEnd cleanArr

main :: IO() --who gives a shit
main = do commandInterpreter