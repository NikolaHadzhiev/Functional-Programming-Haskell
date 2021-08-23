doubleList list = 
    if null list then []
    else (2 * (head list) : (doubleList (tail list)))

removeOdd nums =
    if null nums then []
    else if (mod (head nums) 2 ) == 0
        then (head nums) : (removeOdd (tail nums))
    else removeOdd (tail nums)

listLength [] = 0
listLength list = findLength 1 list
findLength length list =
    if null list
        then (length - 1)
    else findLength (length + 1) (tail list)

createList start end = createListLoop [] start end

createListLoop list start end =
    if start > end then list
    else createListLoop (list ++ [start]) (start + 1) end

createReverseList start end = createReverseListLoop [] start end

createReverseListLoop list start end =
    if start > end then list
    else createReverseListLoop (start : list) (start + 1) end

intsFrom n = n : (intsFrom (n+1))
ints = intsFrom 1
--take 10 ints

manyNumbers = take 10 (repeat 2)

manyNumbers2 = replicate 10 2

cycleList = take 10 (cycle [1, 2, 3, 4, 5])

listTimes2 = [x * 2 | x <- [1..10]]

--listTimes3 gives the result that is less than 50; the numbers are filtered
listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50]

--`mod` or mod is like % in C#
divisibleList = [x | x <- [1..500], x `mod` 9 == 0]

sumOfList = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]

listBiggerThan5 = filter (>5) [4, 7, 10, 15]

multiplyNumbersInList = foldl (*) 1 [1,2,3,4,5]

powList = [3 ^ n | n <- [1..10]]