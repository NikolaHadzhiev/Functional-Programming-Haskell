main :: IO ()
main = do
    let arr = [1,2,3,4,5]
    let list = [1,1,1,1,1,1,1,1,1,1,1,2,3,4,5,5,7,8,1]


    print $mathExpression arr
    print $mathExpressionWithOutFold arr
    print $mathExpressionLastFirst arr
    print $removeSimilar list



mathExpression :: [Int] -> String
mathExpression [] = ""
mathExpression arr =
    foldl (\x y -> concat["(", x, " + ", y, ")"]) (show(head arr)) (map show(tail arr))

mathExpressionWithOutFold [] = ""
mathExpressionWithOutFold arr =
    if null arr 
        then ""
    else if null (tail arr)
        then show(head arr)
    else    
        "(" ++ mathExpressionWithOutFold (tail arr) ++ " + " ++ show (head arr) ++ ")"


mathExpressionLastFirst :: [Int] -> String
mathExpressionLastFirst [] = ""
mathExpressionLastFirst arr =
    foldr (\x y -> concat["(", x, " + ", y, ")"]) (show(last arr)) (map show(init arr))


removeSimilar :: [Int] -> [Int]
removeSimilar [] = []
removeSimilar arr =
    foldl (\accumulator element -> removeSimilarInner accumulator element) [] arr
removeSimilarInner array element =
    if null array
        then array ++ [element]
    else if  last array == element
        then array
    else array ++ [element]

dublicateArr :: [Int] -> String
dublicateArr [] = ""
dublicateArr arr =
    foldl (\acc elem -> concatHelper acc elem 2) [] (map show arr)

concatHelper arr elem n = do
    if n < 0
        then arr
    else 
        concatHelper (arr ++ elem) elem (n - 1)

trimArr :: [Int] -> Int -> Int -> [Int]
trimArr [] x  y = []
trimArr arr startIndex endIndex 
    | endIndex <= 0 = []
    | startIndex == endIndex = []
    | otherwise = trimArrInner (drop startIndex arr) (endIndex - startIndex)
trimArrInner :: [Int] -> Int -> [Int]
trimArrInner arr endIndex = 
    take (endIndex + 1) arr