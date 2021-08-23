main :: IO()

forLoop i condition step body =
    if not (condition i) then return ()
    else do 
        body i
        forLoop (i + step) condition step body
    
myPrint i =
    print i

main = do
    forLoop 0 (\i -> i < 10) 1 myPrint
    putStrLn ( show sumNumbers)

sumNumbers = sumNumbersLoop 0 1 10
sumNumbersLoop num index maxNum =
        if index > maxNum then num 
        else index + (sumNumbersLoop num (index + 1) maxNum) 


basicLoopString a n =
    if n==0
        then ""
    else
        a ++(basicLoopString a (n-1))  

repeatStringLoop string result n =
    if n == 0 then result
    else repeatStringLoop string (result ++ string) (n - 1)

repeatString string n = repeatStringLoop string string n

factoriel n = findFactoriel n 1 1 
findFactoriel :: (Ord t, Num t) => t -> t -> t -> t
findFactoriel n initialValue index = 
    if index > n then initialValue
    else findFactoriel n (initialValue*index) (index+1)



