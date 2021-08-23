splitInt :: Integer -> [Integer]
splitInt n = map (\x -> read [x] :: Integer) (show n)

minNumFunc :: [Integer] -> Integer
minNumFunc arr = minInner arr 

minInner :: [Integer] -> Integer
minInner arr = do
     let minNumber = foldl (\accumolator element -> findMinNumber accumolator element) (head arr) (tail arr)
     minNumber
    
findMinNumber :: Integer -> Integer-> Integer
findMinNumber element minElement =
    if element < minElement then element
    else minElement

main :: IO()
main = do
  num <- getLine
  let intNum = read num :: Integer
  let splitedInt = splitInt intNum
  let minNum = minNumFunc splitedInt
  print minNum
