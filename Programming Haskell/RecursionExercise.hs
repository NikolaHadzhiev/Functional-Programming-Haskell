main :: IO()

main = do
    putStrLn (show (sumPrimeNums 2))

repeatString :: [Char] -> Integer -> [Char]
repeatString  = repeatMeTwo "" 

repeatMeTwo :: (Ord t, Num t) => [a] -> [a] -> t -> [a]
repeatMeTwo str char n = 
    if n <= 0
    then str
    else
        repeatMeTwo (str ++ char) char (n - 1) 

printTriangle str n = do
    if n <= 0
    then return ()
    else do
        putStrLn (repeatString str n)
        printTriangle str (n - 1) 
        putStrLn (repeatString "#" n)
    

sumPrimeNums input =
    sumPrimeNumsTwo 0 2 input

sumPrimeNumsTwo currentSum currentPrime n = do
    if n <= 0
    then currentSum
    else
        sumPrimeNumsTwo (currentSum + currentPrime) (findNextPrime currentPrime) (n - 1) 
    
findNextPrime :: Int -> Int
findNextPrime currentPrime =
    if isPrime (currentPrime + 1)
    then currentPrime + 1
    else
        findNextPrime (currentPrime + 1)

isPrime :: Int -> Bool
isPrime currentPrime = 
    isPrimeTwo currentPrime (currentPrime - 1)

isPrimeTwo :: Int -> Int -> Bool
isPrimeTwo n current = do
    if current == 1
    then True
    else if (n `mod` current) == 0
    then False
    else isPrimeTwo n (current - 1)


concatToArr :: [a] -> [a] -> [a]
concatToArr arr newElementArr = do
    arr ++ newElementArr

reverseArrInner :: [a] -> [a]
reverseArrInner array = do
    if length array <= 1
    then array
    else do
        concatToArr (reverseArrInner (tail array)) [head array]
        
reverseArr :: [a] -> [a]
reverseArr array = do
    reverseArrInner array

ls :: [Int]
ls = [1, 2, 3, 4, 5]


