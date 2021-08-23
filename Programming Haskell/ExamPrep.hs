main :: IO ()

decendingSort :: Ord a => [a] -> [a]
decendingSort [] = []
decendingSort arr = decendingSortInner arr

decendingSortInner arr = do
    let lesserArray = filter (< head arr) (tail arr)
    let greaterArray = filter (>= head arr) (tail arr)
    if null arr then []
    else decendingSortInner greaterArray ++ [head arr] ++ decendingSortInner lesserArray
    
main = do
    print $ decendingSort ["asd", "bsd", "csd"]






