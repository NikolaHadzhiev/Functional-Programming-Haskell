main :: IO()

main = do
    print (fibonacci 21)
    print $ log2 10
    print $ logN 10000
    print $ splitString "blahrow" "row"

log2 :: Int -> Int
log2 n = 
    if n <= 1 then 0
    else 1 + log2 (n `div` 2)

logN n = length (takeWhile (<n) (iterate (*2) 2))

factorial n = findFactorial n 1 1

findFactorial n result index =
    if index > n then result
    else findFactorial n (result * index) (index + 1)


fibonacci :: (Ord t1, Num t2, Num t1) => t1 -> t2
fibonacci n = findFibonacci n 1 0 1 

findFibonacci n result prevNum index = 
    if index >= n then result
    else findFibonacci n (result + prevNum) (result) (index + 1)

splitString :: [Char] -> [Char] -> [Char]
splitString str splitter = splitStringInner str splitter [] (length str) 

splitStringInner str splitter result index =
     if index == 0
        then result
     else if (head str) /= splitter then splitStringInner (tail str) splitter (result ++ [head str]) (index - 1)
     else splitStringInner (tail str) splitter (result) (index - 1)


