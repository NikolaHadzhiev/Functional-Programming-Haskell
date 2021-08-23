main :: IO()

main = do
    print $ mathExpression [1,2,3,4,5]
    print $ mathExpression2 [1,2,3,4,5]
    print $ foldl (\accumolator element -> accumolator ++ element) "asd" ["1", "2", "3", "4", "5"]
    print $ foldl (\x y -> x + y) 0 [1, 2, 3, 4, 5]
    print $ foldl (\x y -> y ++ x) "" ["1", "2", "3", "4", "5"]
    print $ foldr (\accumolator element -> accumolator ++ element) "asd" ["1", "2", "3", "4", "5"]
    let list = [0, 1, 2, 8, 4, 5, 9, 7, 15, 2]
    print (foldl max (head list) list) -- prints last maximum number 
    print (funcApplier 3 (\x -> x + 5))
    print $ add5 $ plus5 3
    printSolution
    let list2 = [1,1,1,1,1,1,1,1,1,1,1,1,2,3,4,5,5,7,8,1]
    print $ foldl (\accumolator element -> addIfNotEqualToLast accumolator element) [] list2
    let list3 = [1,2,3,4,5]
    print (subArray list3 1 2)
    print $ dublicateList [1,2,3,4,4]
    print $ replicateList [1,2] 5






--EXECISE 1 -- x -> ["1"] y-> ["2", "3", "4", "5"] -- when x is single in foldl the element "(" is added in the beggining 4 
--times because of the next 4 elements in the list [1, 2, 3, 4, 5]
mathExpression :: [Int] -> String
mathExpression [] = ""
mathExpression arr =
    foldl (\x y -> concat["(", x," + ", y, ")"]) (show (head arr)) (map show (tail arr))

--EXECISE 2 -- x -> ["5"] y-> ["1", "2", "3", "4""] -- when x is single in foldr the element ")" is added in the ending 4 
--times because of the next 4 elements in the list [1, 2, 3, 4, 5]
mathExpression2 :: [Int] -> String
mathExpression2 [] = ""
mathExpression2 arr =
     foldr (\x y -> concat["(", x, " + ", y, ")"]) (show(last arr)) (map show(init arr))

funcApplier x func = func x

plus5 x = x + 5
add5 x = plus5 x

--EXERCISE 1(IVO SOLUTION)
printSolution = do
    let list = [1, 2, 3, 4, 5]
    print $ foldl (\a e -> "(" ++ a ++ "+" ++ (show e) ++ ")") (show (head list)) (tail list)

--EXERCISE 3
addIfNotEqualToLast :: Eq a => [a] -> a -> [a]
addIfNotEqualToLast array element =
    if null array
        then array ++ [element]
    else if element == (last array)
        then array
    else array ++ [element]

--EXERCISE 6
subArray list start end 
    | start > end = []
    | null list = []
    | end >= (length list) = (subArrayInner list start ((length list) - 1) 0 )
    | otherwise = subArrayInner list start end 0

subArrayInner list start end currentIndex =
    if  currentIndex < start
        then subArrayInner (tail list) start end (currentIndex + 1)
    else if currentIndex >= start && currentIndex <= end 
        then [(head list)] ++ subArrayInner (tail list) start end (currentIndex + 1)
    else []

--EXERCISE 4
dublicateList :: [a] -> [a]
dublicateList arr =
    if null arr then []
    else (repeatElement (head arr) 2) ++ (dublicateList (tail arr))

repeatElement element n = repeatElementInner element n []

repeatElementInner element n array =
    if n == 0 then array
    else element : repeatElementInner element (n - 1) array

--EXERCISE 5
replicateList arr n=
    if null arr then []
    else (repeatElementList (head arr) n) ++ (replicateList (tail arr) n)

repeatElementList element n = repeatElementListInner element n []

repeatElementListInner element n array =
    if n == 0 then array
    else element : repeatElementListInner element (n - 1) array