main :: IO ()
main = do
    getNumber
   

getNumber = do
    numberStr <- getLine
    let numArr = getNumArr numberStr []
    let arrSum = sum numArr
    let currentLength = length numArr
    let result = fromIntegral arrSum / fromIntegral currentLength
    let resultFormated = formatFloatN result 0
    let resultFormatedInt = (read resultFormated :: Integer)
    print resultFormatedInt

getNumArr :: [Char] -> [Integer] -> [Integer]
getNumArr number currentArr = do 
    if null number
        then []
    else do
        let currentNum = (read [head number] :: Integer)
        currentArr ++ [currentNum] ++ getNumArr (tail number) currentArr

formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""