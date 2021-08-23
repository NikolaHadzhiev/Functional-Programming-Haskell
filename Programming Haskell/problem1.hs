main :: IO()

main = do
    getAverageOfNum

splitInt :: Integer -> [Int]
splitInt n = map (\x -> read [x] :: Int) (show n)

getAverageOfNum  = getAvergeOfNumInner 

getAvergeOfNumInner = do
    num <- getLine
    let intNum = read num :: Integer 
    let intLenght = length (splitInt intNum)
    let avg = sum (splitInt intNum) `div` intLenght
    print avg

    

