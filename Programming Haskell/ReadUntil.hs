main :: IO()

main = do
    lines <- readUntil "end"
    print lines

readUntilWord end arr = do
    line <- getLine
    if line == end then return arr
    else readUntilWord end (arr ++ [line])

readUntil end = readUntilWord end []


