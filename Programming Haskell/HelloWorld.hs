import System.IO ()
import Data.List ()

main :: IO ()
main = do
    putStrLn "Hello World!"
    line <- getLine
    putStrLn("You said: " ++ line)

doubleEvenNumber y =
            if(y `mod` 2 /= 0)
                then y
                else y * 2
              

--funcition "show" changes anything into a string
add :: Num a => a -> a -> a
add a b = a + b

in_range :: Integer -> Integer -> Integer -> Bool
in_range min max x = x >= min && x <= max

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

simpleTupil = ("Bob Smith", 52)

tupilName = fst simpleTupil
tupilAge = snd simpleTupil

names = ["A", "B", "C"]
fruits = ["banan", "apple", "cherry"]

combineNamesAndFruits = zip names fruits

--guards
whatAge :: Int -> String
whatAge age 
    | (age >= 5) && (age <= 6) = "Kindergatren"
    | (age > 6) && (age <= 10) = "Elm School"
    | (age > 10) && (age <= 14) = "Mid School"
    | otherwise = "Go home"

testAverage :: Double -> Double -> String
testAverage hits atBats
    |avg <= 0.200 = "Bad"
    |avg <= 0.250 = "Middle"
    |avg <= 0.300 = "Good"
    |otherwise = "Nice"
    where avg = hits/atBats

getListItems :: [Int] -> [Char]
getListItems [] = "Empty"
getListItems (x:[]) = show x
getListItems(x:y:[]) = show x ++ show y
getListItems(x:xs) = "The first is:" ++ show x ++ "others are" ++ show xs

getFirstItemString:: String -> String
getFirstItemString [] = "Empty"
getFirstItemString all@(x:xs) = "The first letter in" ++ all ++ "is" ++ [x]

areStringEquals:: [Char] -> [Char] -> Bool
areStringEquals [][] = True
areStringEquals _ _ = False
areStringEquals(x:xs) (y:ys) = x == y && areStringEquals xs ys

--doMult is going to recieve a function that gets and return int and as a result it is going to return int
times4 :: Int -> Int
times4 x = x * 4

doMult :: (Int -> Int) -> Int
doMult func = func 3

num3Times4 = doMult times4

getClass:: Int -> String
getClass n = case n of
       5 -> "Go to Kindergarten"
       6 -> "Go to elementary"
       _ -> "Go away"


data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper Beats Scissors"
