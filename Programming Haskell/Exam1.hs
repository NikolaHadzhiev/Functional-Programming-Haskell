main :: IO()

main = do
    getAverageOfNum

splitInt :: Integer -> [Int]
splitInt n = map (\x -> read [x] :: Int) (show n)

getAverageOfNum  = getAvergeOfNumInner 

getAvergeOfNumInner = do
    num <- getLine
    let intNum = read num :: Integer 
    let avg = sum (splitInt intNum) `div` length (splitInt intNum)
    print avg

    

