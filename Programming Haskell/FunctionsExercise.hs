main :: IO()

main = do
    print $ abstThroughFunction 10 10 firstFunc

abstThroughFunction a b func = func a b
firstFunc a b = (a * b)
secondFunc a b = (a + b)
thirdFunc a b = (a - b)

--absoluteList list = map abs list -- [1,2,-3,-4] --> [1,2,3,4]
--plus1List list = map (1 + ) list -- [1,2,3,4,5] -- [2,3,4,5,6]
--isEven x = x `mod` 2 == 0; removeOdd = filter isEven  --removeOdd [1,2,3,4,5,6,7,8] -- [2,4,6,8]
--foldl(\accumulator element -> accumulator ++ element) -- asd12345
--foldr(\accumulator element -> accumulator ++ element) -- 12345asd
--subtractList list = foldl (-) 0 list -- subtractList [1,2,3,4,5] -- -15
--subtractList2 list = foldr (-) 0 list -- subtractList2 [1,2,3,4,5] -- 3
--zip [1,3,5] [2,4,6] -- [(1,2),(3,4),(5,6)]
--zip [1,2] [3,4,5,6] -- [(1,3),(2,4)]
--zip [] [1] -- []
--zipWith (+) [1,2,3,4,5] [9,8,7,6,5] -- [10,10,10,10,10]
--maxFromList list = foldl max (head list) list -- maxFromList [-1, 5, 10] -- 10
--addOneList list = map (\x -> x + 1) list --addOneList [1,1,1] -- [2,2,2]
--zipWith (\ x y -> x + y ) [10,12] [3,4] --[13,16]




