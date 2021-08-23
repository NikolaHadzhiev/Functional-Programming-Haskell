main :: IO()

main = do
    print $ itterateArray [1, 2, 3] 5 (\x y -> repeatElement x y)

getNElementFromList [] _ = error "Empty list"
getNElementFromList list n = listLoop list (length list) n 0
listLoop list listLenght n index = 
    if n >= listLenght || n < 0 then error "Out of range"
    else if index == n then (head list)
    else listLoop (tail list) listLenght n (index + 1)

ls :: [Int]
ls = [1, 2, 3, 4, 5]

filterArray array number =
    if null array then []
    else if (mod (head array) number) == 0
        then (filterArray (tail array) number)
    else (head array) : (filterArray (tail array) number)

enumerate10Numbers:: (Eq a, Num a) => a-> a -> [a]
enumerate10Numbers start lenght =
    if lenght == 0 
        then []
    else start : (enumerate10Numbers (start + 1) (lenght - 1))

enumerate start length =
    if start == length
        then []
    else start : (enumerate (start + 1) length)

enumerateWithStep start lenght step =
    if lenght == 0 then []
    else start : (enumerateWithStep (start + step) (lenght - 1) step)

reverseArray array = 
    if null array then []
    else reverseArray (tail array) ++ [head array]

getElementByIndex [] _ = error "Array is empty"
getElementByIndex array index =
    if index < 0 || index > (length array)
        then error "Invalid index"
    else if index == 0
        then (head array)
    else getElementByIndex (tail array) (index - 1)

repeatElement element n = repeatElementInner element n []
repeatElementInner element n array =
    if n == 0
        then array
    else element : repeatElementInner element (n - 1) array

dublicateArr arr n =
    if null arr then []
    else (repeatElement (head arr) n) ++ (dublicateArr (tail arr) n)

itterateArray arr n func =
    if null arr then []
    else (func (head arr) n) ++ (itterateArray (tail arr) n func)




