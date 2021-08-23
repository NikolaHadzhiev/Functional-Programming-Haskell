main :: IO ()
main = do
    num <- getLine
    let intNum = map (:[]) num
    let parse = map (read:: String -> Int) intNum
    print $ minElement parse

minElement :: [Int] -> Int
minElement [] = 0
minElement [n] = n
minElement(n1:n2:n3)
    |n1 > n2 = minElement (n2 : n3)
    |n1 < n2 = minElement (n1 : n3)
    |n1 == n2 = minElement (n1 : n3)

