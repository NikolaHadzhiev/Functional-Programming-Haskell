main :: IO()

main = do
    print $ multiplyNumbers 5 10
    print $ biggestNumber 5 10 (-20)
    print $ doSth 5 add1
    print $ doSth 5 remove1
    print $ mathExpression [1,2,3,4,5]

--EXERCISE 1
multiplyNumbers a = \b -> a * b

--EXERCISE 2
biggestNumber a b = \c -> if a >= b && a >= c then a else if b >= a && b >= c then b else c

--EXERCISE 3
doSth n = \func -> func n


add1 n = n + 1
remove1 n = n - 1

--EXERCISE 4
mathExpression :: [Int] -> String
mathExpression [] = ""
mathExpression arr =
    foldl (\x y -> concat["(", x," + ", y, ")"]) (show (head arr)) (map show (tail arr))