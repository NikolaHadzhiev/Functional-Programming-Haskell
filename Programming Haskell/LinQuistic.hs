
{- Split -}

splitAtSection :: String -> Int -> [Int]
splitAtSection str start = if start == length str    then [start, 0]
                           else if str!!start == '.' then [start, 1]
                           else if str!!start == '(' then [start, 3]
                           else splitAtSection str (start + 1)

splitInput :: String -> [String]
splitInput line = if null line then []
                  else [take (head (splitAtSection line 0)) line] ++ splitInput (drop (sum (splitAtSection line 0)) line)

{- Insert -}

sanitizeArr :: Eq a => [a] -> [a]
sanitizeArr arr = if null arr then []
                  else if elem (last arr) (init arr) then sanitizeArr (init arr)
                  else (sanitizeArr (init arr)) ++ [last arr]

insertIncl :: [[String]] -> [String] -> [[String]]
insertIncl mat info = if indexOfHead mat (head info) 0 < 0 
                      then mat ++ [[head info] ++ sanitizeArr (tail info)]
                      else insertInfo mat (indexOfHead mat (head info) 0) (tail info) 
    where insertInfo mat index info = take index mat ++ [[head (mat!!index)] ++ sanitizeArr ((tail (mat!!index)) ++ info)] ++ drop (index + 1) mat 

{- Sort -}

desc :: (Ord a) => [a] -> (a -> a -> Bool) -> (a -> a -> Bool) ->  Int -> Int -> [a]
desc arr cond scond end curr = if end == 0 then arr
                               else if curr == end then desc arr cond scond (end - 1) 0
                               else if cond (arr!!curr) (arr!!(curr + 1)) || scond (arr!!curr) (arr!!(curr+1)) 
                                   then desc (swapTwo arr curr) cond scond end (curr + 1)
                               else desc arr cond scond end (curr + 1)
    where swapTwo arr i = take i arr ++ [arr!!(i + 1)] ++ [arr!!i] ++ drop (i + 2) arr 

orderByDesc :: (Ord a) => [a] -> (a -> a -> Bool) -> (a -> a -> Bool) -> [a]
orderByDesc arr cond scond = desc arr cond scond (length arr - 1) 0

{- Sort conditions -}

compByUniqCnt :: String -> String -> Bool
compByUniqCnt x y = length x == length y && uniqCnt x < uniqCnt y  
    where uniqCnt str = length (foldl (\acc x -> if elem x acc then acc else acc ++ [x]) [] str)

compByLen :: (Ord a) => [a] -> [a] -> Bool
compByLen x y = length x < length y

compByShort :: (Ord a) => [a] -> [a] -> Bool
compByShort x y = length x > length y

compByShortestName :: [String] -> [String] -> Bool 
compByShortestName x y = length (last (orderByDesc (tail x) compByLen alwaysFalse)) < length (last (orderByDesc (tail y) compByLen alwaysFalse))

alwaysFalse :: a -> a -> Bool
alwaysFalse x y = False

{- Filters -}

elemsThatContain :: [[String]] -> String -> [[String]]
elemsThatContain mat item = if null mat then []
                            else if elem item (head mat) then [head mat] ++ elemsThatContain (tail mat) item
                            else elemsThatContain (tail mat) item

getCollWithMostMethods :: [[String]] -> [String]
getCollWithMostMethods mat = mat!!(indexOfMostMethods lengthsArr)
    where lengthsArr = map (\x -> length x) mat 
          indexOfMostMethods arr = foldl (\acc x -> if arr!!x > arr!!acc then x else acc) 0 [1..(length mat - 1)]   

{- Printing -}

printMethods arr =
    if null arr then return ()
    else do
        putStrLn ("* " ++ (head arr))
        printMethods (tail arr)

printCollection :: [[String]] -> Bool -> IO ()
printCollection collection haveMethods = do
    if null collection then return ()
    else do
        putStrLn (head (head collection))
        if haveMethods 
            then printMethods (orderByDesc (tail (head collection)) compByLen alwaysFalse) 
        else return ()
        printCollection (tail collection) haveMethods

{- Main -}

indexOfHead :: [[String]] -> String -> Int -> Int
indexOfHead mat key start = if null mat then (-1)
                            else if head (head mat) == key then start
                            else indexOfHead (tail mat) key (start + 1)

isNumber :: String -> Bool
isNumber str = if null str then False
               else (head str >= '0' && head str <= '9') && ((length str > 1) == isNumber (tail str))

readUntilWord :: [[String]] -> IO ()
readUntilWord mat = do
    line <- getLine
    let coll = splitInput line

    if head coll == "exit" then do
        final <- getLine
        let comm = words final
        let collecs = elemsThatContain mat (head comm)
        let ordered = orderByDesc collecs compByLen compByShortestName
            
        printCollection ordered (comm!!1 == "all")

    else if length coll == 1 then do
        if isNumber (head coll) then do
            let mostMethodsOrd = orderByDesc (tail (getCollWithMostMethods mat)) compByShort alwaysFalse
            let n = read (head coll) :: Int
            printMethods (take n mostMethodsOrd)

        else if indexOfHead mat (head coll) 0 >= 0 then do
            let methods = orderByDesc (tail (mat!!(indexOfHead mat (head coll) 0))) compByLen compByUniqCnt 
            printMethods methods

        else return()
        readUntilWord mat

    else readUntilWord (insertIncl mat coll)

main :: IO()
main = do
    readUntilWord []
