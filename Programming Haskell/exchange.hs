main :: IO()

main = do 
    exchange [1, 3, 5, 7, 9] 1
    maxOdd [1, 3, 5, 7, 9]
    maxEven [2, 4, 6, 8, 10]
    minOdd [1, 3, 5, 7, 9]
    minEven [2, 4, 6, 8, 10]
    firstNOdd [4, 8, 2, 6] 3
    firstNEven [1, 8, 2, 4] 4
    lastNOdd [1, 8, 2, 3, 5] 4
    lastNEven [1, 3, 5, 7, 9] 5

exchange arr index = exchangeInner arr [] index
exchangeInner arr result index = do
      if index > length arr || index + 1 > length arr then print "Invalid index"
      else print (result ++ drop (index + 1) arr ++ take (index + 1) arr)

maxOdd arr = maxOddInner arr  
maxEven arr = maxEvenInner arr
maxOddInner arr = do
     let filterArr = [x | x <- arr, x `mod` 2 == 1]
     if null filterArr then print "No matches"
     else do
     let maxNumber = foldl (\accumolator element -> findMaxNumber accumolator element) (head filterArr) (tail filterArr)
     let maxIndex = getNumberIndex arr maxNumber 
     print maxIndex

maxEvenInner arr = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if null filterArr then print "No matches"
    else do
    let maxNumber = foldl (\accumolator element -> findMaxNumber accumolator element) (head filterArr) (tail filterArr)
    let maxIndex = getNumberIndex arr maxNumber
    print maxIndex

findMaxNumber element maxElement =
    if element > maxElement then element
    else maxElement
findMinNumber element minElement =
    if element < minElement then element
    else minElement

getNumberIndex list n = getNumberIndexInner list (length list) n 0
getNumberIndexInner list listLenght n index =
    if null (tail list) then index 
    else if head list == head (tail list) && head (tail list) == n then index + 1
    else if head list == n then index
    else getNumberIndexInner (tail list) listLenght n (index + 1)



minOdd arr = minOddInner arr  
minEven arr = minEvenInner arr
minOddInner arr = do
     let filterArr = [x | x <- arr, x `mod` 2 == 1]
     if null filterArr then print "No matches"
     else do
     let minNumber = foldl (\accumolator element -> findMinNumber accumolator element) (head filterArr) (tail filterArr)
     let minIndex = getNumberIndex arr minNumber 
     print minIndex

minEvenInner arr = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if null filterArr then print "No matches"
    else do
    let minNumber = foldl (\accumolator element -> findMinNumber accumolator element) (head filterArr) (tail filterArr)
    let minIndex = getNumberIndex arr minNumber
    print minIndex

firstNOdd arr index = firstNOddInner arr index

firstNOddInner arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 1]
    if null filterArr then print ([] :: [Int])
    else do
        if index > (length arr) then print "Invalid Count"
        else do
        let firstArr = findFirstNNumbers filterArr index 0 []
        print firstArr

findFirstNNumbers arr index n result =
    if null arr then result
    else if n == index then result
    else findFirstNNumbers (tail arr) index (n + 1) (result ++ [head arr])

firstNEven arr index = firstNEvenInner arr index

firstNEvenInner arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if null filterArr then print ([] :: [Int])
    else do
        if index > (length arr) then print "Invalid Count"
        else do
        let firstArr = findFirstNNumbers filterArr index 0 []
        print firstArr

lastNOdd arr index = lastNOddInner arr index
lastNOddInner arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 1]
    if null filterArr then print ([] :: [Int])
    else do
        if index > (length arr) then print "Invalid Count"
        else do
        let reverseArr = reverse filterArr
        let firstArr = findFirstNNumbers reverseArr index 0 []
        print (reverse firstArr)

lastNEven arr index = lastNEvenInner arr index
lastNEvenInner arr index = do
    let filterArr = [x | x <- arr, x `mod` 2 == 0]
    if null filterArr then print ([] :: [Int])
    else do
        if index > (length arr) then print "Invalid Count"
        else do
        let reverseArr = reverse filterArr
        let firstArr = findFirstNNumbers reverseArr index 0 []
        print (reverse firstArr)

