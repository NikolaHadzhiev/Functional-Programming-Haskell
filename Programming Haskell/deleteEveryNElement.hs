deleteEveryNElement num arr = deleteEveryNElementInner num arr


deleteEveryNElementInner num arr = do
    if length arr < num-1 then arr 
    else do 
        take (num - 1) arr ++ deleteEveryNElementInner num (drop num arr)

main ::IO()

main = do
    print $ deleteEveryNElement 2 [1,2,3,4,5,6]