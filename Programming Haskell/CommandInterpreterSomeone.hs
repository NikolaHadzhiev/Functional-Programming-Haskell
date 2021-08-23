import Data.List (sort)

validParameters :: Int -> Int -> [a] -> Bool
validParameters start count arr = start >= 0 && start < length arr && count >= 0 && start + count <= length arr 

reverseArr :: Int -> Int -> [a] -> [a]
reverseArr start count arr = take start arr ++ foldl (\array a -> [a] ++ array) [] (take count (drop start arr)) ++ drop (start + count) arr 

sortArr :: (Ord a) => Int -> Int -> [a] -> [a]
sortArr start count arr = take start arr ++ sort (take count (drop start arr)) ++ drop (start + count) arr

rollLeft :: Int -> [a] -> [a]
rollLeft count arr = if count == 0 then arr
                     else rollLeft (count - 1) ((tail arr) ++ [head arr])

rollRight :: Int -> [a] -> [a]
rollRight count arr = if count == 0 then arr
                      else rollRight (count - 1) ([last arr] ++ (init arr))
 
doError :: [String] -> IO [String]
doError arr = do
    print "Invalid input parameters."
    readUntilWord arr

readUntilWord :: [String] -> IO [String]
readUntilWord arr = do
    line <- getLine
    let comms = words line

    if null comms then readUntilWord arr 
    else
        if head comms == "reverse" || head comms == "sort" then do
            let start = read (comms!!2) :: Int
            let count = read (comms!!4) :: Int 

            if validParameters start count arr then 
                if head comms == "reverse" then
                    readUntilWord (reverseArr start count arr)
                else readUntilWord (sortArr start count arr)
            else doError arr

        else if head comms == "rollLeft" || head comms == "rollRight" then do
            let count = read (comms!!1) :: Int

            if count >= 0 then 
                if head comms == "rollLeft" then
                    readUntilWord (rollLeft (mod count (length arr)) arr)
                else readUntilWord (rollRight (mod count (length arr)) arr) 
            else doError arr

    else if head comms == "end" then return arr
    else readUntilWord arr

formatResult :: [[Char]] -> String -- don't ask me why arr is [[Char]]
formatResult arr = (foldl (\str a -> str ++ a ++ ", ") "[" (init arr)) ++ last arr ++ "]"

main :: IO()
main = do   
    line <- getLine
    let nums = words line 
    result <- readUntilWord nums
    putStrLn (formatResult result)