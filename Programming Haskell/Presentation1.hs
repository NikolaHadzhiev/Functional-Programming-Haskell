main :: IO()

main = do
    line <- dummyGetLine
    putStrLn (line)
    --putStrLn (repeatStr "H" 5)
     --firstName <- getLine
    -- lastName <- getLine
    -- putStrLn (firstAndLastName firstName lastName)
     --num1 <- getLine
     --num2 <- getLine
     --let parsedNum1 = read num1 :: Int
     --let parsedNum2 = read num2 :: Int
     --let answer = multiply parsedNum1 parsedNum2
     --putStrLn (show answer)
      -- radius <- getLine
      -- let parsedRadius = read radius :: Float
      -- let param = circleMath parsedRadius
      -- putStrLn (show param)
     -- print (doubleVal 5)
     --print (biggestOf3 15 24 11)
      -- print(execute add1 5)



dummyGetLine :: IO String
dummyGetLine = return "Hello World"

repeatStr :: (Eq n, Num n) => [Char] -> n -> [Char]
--repeatStr :: String -> Int -> String
repeatStr str n = 
    if n == 0 then ""
    else str ++ (repeatStr str (n - 1))

firstAndLastName :: String -> String -> String
firstAndLastName firstName lastName =
    firstName ++ " " ++ lastName

multiply :: Num a => a -> a -> a
multiply num1 num2 = num1 * num2

circleMath :: Floating a => a -> a
circleMath r = pi * r * r

doubleVal :: Num a => a -> a
doubleVal num = num + num 

isEven :: Integral a => a -> Bool
isEven num 
        | num `mod` 2 == 0 = True
        | otherwise = False

biggestOf3 :: Ord a => a -> a -> a -> a
biggestOf3 a b c
            | (a > b) && (a > c) = a
            | (b > a) && (b > c) = b
            | otherwise = c

add1 num = num + 1
remove1 num = num - 1

execute :: (Int -> Int) -> Int -> Int
execute func num = func num

factorial num =
    if(num == 0 || num == 1) then 1
    else num * factorial (num - 1)

fibonacci num =
    if(num == 1 || num == 2) then 1
    else fibonacci (num - 1) + fibonacci (num - 2)






