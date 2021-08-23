main :: IO()
main = do 
    line <- getLine
    let strNums = words line 
    let intNums = map read strNums :: [Int]
    result <- readUntilWord intNums
    print result 

--{EXCHANGE}
exchange :: [Int] -> Int -> [Int]
exchange arr index = exchangeInner arr [] index

exchangeInner :: [Int] -> [Int] -> Int -> [Int]
exchangeInner arr result index = do
      result ++ drop (index + 1) arr ++ take (index + 1) arr
--{END OF EXCHANGE}

--{MAX ODD}
maxOdd ::[Int] -> Int
maxOdd arr = maxOddInner arr 
maxOddInner :: [Int] -> Int
maxOddInner arr = do
     let filterArr = [x | x <- arr, x `mod` 2 == 1]
     if null filterArr then (-1)
     else do
     let maxNumber = foldl (\accumolator element -> findMaxNumber accumolator element) (head filterArr) (tail filterArr)
     let maxIndex = getNumberIndex arr maxNumber 
     maxIndex
--{END OF MAX ODD}

--{MAX EVEN}
maxEven :: [Int] -> Int
maxEven arr = maxEvenInner arr

maxEvenInner :: [Int] -> Int
maxEvenInner arr = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if null filterArr then (-1)
    else do
    let maxNumber = foldl (\accumolator element -> findMaxNumber accumolator element) (head filterArr) (tail filterArr)
    let maxIndex = getNumberIndex arr maxNumber
    maxIndex
--{END OF MAX EVEN}

getNumberIndex ::  [Int] -> Int -> Int
getNumberIndex list n = getNumberIndexInner list (length list) n 0

getNumberIndexInner :: [Int] -> Int -> Int -> Int -> Int
getNumberIndexInner list listLenght n index =
    if null (tail list) then index 
    else if head list == head (tail list) && head (tail list) == n then index + 1
    else if head list == n then index
    else getNumberIndexInner (tail list) listLenght n (index + 1)

findMaxNumber ::  Int -> Int -> Int
findMaxNumber element maxElement =
    if element > maxElement then element
    else maxElement

findMinNumber :: Int -> Int -> Int
findMinNumber element minElement =
    if element < minElement then element
    else minElement

--{MIN ODD}
minOdd :: [Int] -> Int
minOdd arr = minOddInner arr 

minOddInner :: [Int] -> Int
minOddInner arr = do
     let filterArr = [x | x <- arr, x `mod` 2 == 1]
     if null filterArr then (-1)
     else do
     let minNumber = foldl (\accumolator element -> findMinNumber accumolator element) (head filterArr) (tail filterArr)
     let minIndex = getNumberIndex arr minNumber 
     minIndex
--{END OF MIN ODD}

--{MIN EVEN}
minEven :: [Int] -> Int
minEven arr = minEvenInner arr
minEvenInner :: [Int] -> Int
minEvenInner arr = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if null filterArr then (-1)
    else do
    let minNumber = foldl (\accumolator element -> findMinNumber accumolator element) (head filterArr) (tail filterArr)
    let minIndex = getNumberIndex arr minNumber
    minIndex
--{END OF MIN EVEN}

--{FIRST ODD}
firstNOdd ::  [Int] -> Int -> [Int]
firstNOdd arr index = firstNOddInner arr index

firstNOddInner :: [Int] -> Int -> [Int]
firstNOddInner arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 1]
    if null filterArr then ([] :: [Int])
    else do
        let firstArr = findFirstNNumbers filterArr index 0 []
        firstArr
--{END OF FIRST ODD}

findFirstNNumbers :: [Int] -> Int -> Int -> [Int] -> [Int]
findFirstNNumbers arr index n result =
    if null arr then result
    else if n == index then result
    else findFirstNNumbers (tail arr) index (n + 1) (result ++ [head arr])

--{FIRST EVEN}
firstNEven :: [Int] -> Int -> [Int]
firstNEven arr index = firstNEvenInner arr index

firstNEvenInner :: [Int] -> Int -> [Int]
firstNEvenInner arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if null filterArr then ([] :: [Int])
    else do
        let firstArr = findFirstNNumbers filterArr index 0 []
        firstArr
--{END OF FIRST EVEN}

--{LAST ODD}
lastNOdd :: [Int] -> Int -> [Int]
lastNOdd arr index = lastNOddInner arr index

lastNOddInner :: [Int] -> Int -> [Int]
lastNOddInner arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 1]
    if null filterArr then ([] :: [Int])
    else do
        let reverseArr = reverse filterArr
        let firstArr = findFirstNNumbers reverseArr index 0 []
        (reverse firstArr)
--{END OF LAST ODD}

--{LAST EVEN}
lastNEven :: [Int] -> Int -> [Int]
lastNEven arr index = lastNEvenInner arr index

lastNEvenInner :: [Int] -> Int -> [Int]
lastNEvenInner arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if null filterArr then ([] :: [Int])
    else do
        let reverseArr = reverse filterArr
        let firstArr = findFirstNNumbers reverseArr index 0 []
        reverse firstArr
--{END OF LAST EVEN}

--{READ UNTIL}
readUntilWord :: [Int] -> IO [Int]
readUntilWord arr = do
    line <- getLine
    let comms = words line

    if null comms then readUntilWord arr 
    else if head comms == "exchange" then do
            let index = read (comms!!1) :: Int
            if filterExchange arr index then 
                    readUntilWord (exchange arr index)
            else doExchangeError arr
    else if head comms == "max" then do
             let oddOrEven = comms!!1
             if oddOrEven == "odd" then do
                 if maxOdd arr /= -1 then
                     printMaxOdd arr
                 else doMaxMinError arr
             else if oddOrEven == "even" then do 
                 if maxEven arr /= -1 then
                     printMaxEven arr
                 else doMaxMinError arr
             else readUntilWord arr 
    else if head comms == "min" then do
            let oddOrEven = comms!! 1
            if oddOrEven == "odd" then do
                if minOdd arr /= -1 then
                    printMinOdd arr
                else doMaxMinError arr
            else if oddOrEven == "even" then do
                if minEven arr /= -1 then
                     printMinEven arr
                else doMaxMinError arr
            else readUntilWord arr 
    else if head comms == "first" then do
            let index = read (comms!!1) :: Int
            let oddOrEven = comms!!2
            if oddOrEven == "odd" then do
                if filterFirstOdd arr index then
                    printFirstOdd arr index 
                else doFirstOddError arr 
            else if oddOrEven == "even" then do
                if filterFirstEven arr index then
                    printFirstEven arr index 
                else doFirstEvenError arr 
            else readUntilWord arr
    else if head comms == "last" then do
            let index = read (comms!!1) :: Int
            let oddOrEven = comms!!2
            if oddOrEven == "odd" then do
                if filterLastOdd arr index then
                    printLastOdd arr index 
                else doLastOddError arr 
            else if oddOrEven == "even" then do
                if filterLastEven arr index then 
                    printLastEven arr index 
                else doLastEvenError arr 
            else readUntilWord arr
    else if head comms == "end" then return arr
    else readUntilWord arr
--{END OF READ UNTIL}

--{ERRORS}
doExchangeError :: [Int] -> IO [Int]
doExchangeError arr = do
    print "Invalid index"
    readUntilWord arr

doMaxMinError :: [Int] -> IO [Int]
doMaxMinError arr = do
    print "No matches"
    readUntilWord arr

doFirstOddError arr = do
    let filterArr = [x | x <- arr, x `mod` 2 == 1]
    if null filterArr then do
         print ([] :: [Int])
         readUntilWord arr
    else do 
        print "Invalid Count"
        readUntilWord arr

doFirstEvenError arr  = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if null filterArr then do
         print ([] :: [Int])
         readUntilWord arr
    else do 
        print "Invalid Count"
        readUntilWord arr

doLastOddError arr  = do
    let filterArr = [x | x <- arr, x `mod` 2 == 1]
    if null filterArr then do
         print ([] :: [Int])
         readUntilWord arr
    else do 
        print "Invalid Count"
        readUntilWord arr
    
doLastEvenError arr  = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if null filterArr then do
         print ([] :: [Int])
         readUntilWord arr
    else do 
        print "Invalid Count"
        readUntilWord arr
--{END OF ERRORS}

--{PRINT METHODS}
printMaxOdd :: [Int] -> IO [Int]
printMaxOdd arr = do
    print $ maxOdd arr
    readUntilWord arr

printMaxEven :: [Int] -> IO [Int]
printMaxEven arr = do
    print $ maxEven arr
    readUntilWord arr

printMinOdd :: [Int] -> IO [Int]
printMinOdd arr = do
    print $ minOdd arr
    readUntilWord arr

printMinEven :: [Int] -> IO [Int]
printMinEven arr = do
    print $ minEven arr
    readUntilWord arr

printFirstOdd :: [Int] -> Int -> IO [Int]
printFirstOdd arr index = do
    print $ firstNOdd arr index 
    readUntilWord arr

printFirstEven :: [Int] -> Int -> IO [Int]
printFirstEven arr index = do
    print $ firstNEven arr index
    readUntilWord arr

printLastOdd :: [Int] -> Int -> IO [Int]
printLastOdd arr index= do
    print $ lastNOdd arr index 
    readUntilWord arr

printLastEven :: [Int] -> Int -> IO [Int]
printLastEven arr index= do
    print $ lastNEven arr index 
    readUntilWord arr
--{END OF PRINT METHODS}

filterExchange arr index = do
    if index > length arr || index + 1 > length arr then False
    else True

filterFirstOdd arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 1]
    if index > length arr then False
    else if null filterArr then False
    else True

filterFirstEven arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if index > length arr then False
    else if null filterArr then False
    else True

filterLastOdd :: Integral a => [a] -> Int -> Bool
filterLastOdd arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 1]
    if index > length arr then False
    else if null filterArr then False
    else True
filterLastEven arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if index > length arr then False
    else if null filterArr then False
    else True
