main :: IO()

main = do
   print $ func1 1 2 3
   print $ func2 1 2 3
   print $ func3 1 2 3
   print $ func4 1 2 3

func1 a b c = a * b + c

func2 a b = \ c -> a * b + c 

func3 a b = let product = a * b in \ c -> product + c 

func4 a = \ b -> let product = a * b in \ c -> product + c

