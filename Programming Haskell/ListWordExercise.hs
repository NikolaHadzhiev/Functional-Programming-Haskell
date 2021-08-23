main :: IO()

-- EXERCISE 4
removeNthElementInner :: (Eq t, Num t) => [a] -> Int -> t -> [a]
removeNthElementInner arr n index = 
    if index == 0
        then []
    else (take (n - 1) arr) ++ (removeNthElementInner (drop n arr) n (index - 1))

removeNthElement :: [a] -> Int -> [a]
removeNthElement arr n = removeNthElementInner arr n n

main = do
    print $ removeNthElement [1, 2, 3, 4, 5, 6, 7, 8, 9] 3
    print $ reverseList [1, 2, 3, 4]
    print $ listLenght "Hello, world!"
    print $ dublicateList [1,2,3]
    print $ fibonacci 10
    print $ factorial 10

-- EXERCISE 1
reverseList :: [Int] -> [Int]
reverseList arr = 
    if null arr then []
    else reverseList (tail arr) ++ [head arr]

-- EXERCISE 2
findListLenght lenght list =
    if null list then (lenght - 1)
    else findListLenght (lenght + 1) (tail list)

listLength [] = 0
listLenght list = findListLenght 1 list

-- EXERCISE 3
dublicateList arr =
    if null arr then []
    else (repeatElement (head arr) 2) ++ (dublicateList (tail arr))

repeatElement element n = repeatElementInner element n []

repeatElementInner element n array =
    if n == 0 then array
    else element : repeatElementInner element (n - 1) array

--EXERCISE 5
fibonacci n = findFibonacci n 1 0 1 []

findFibonacci n result prevNum index arr =
    if index > n then arr
    else findFibonacci n (result + prevNum) (result) (index + 1) (arr ++ [result])

--EXERCISE 6
factorial n = findFactorial n 1 1 []

findFactorial n result index arr =
    if index > n then arr
    else findFactorial n (result * index) (index + 1) (arr ++ [result])