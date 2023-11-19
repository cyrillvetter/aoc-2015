import Debug.Trace (trace)

input = 36000000

main = do
    print $ countPresents 1
    print "Day 20"

countPresents :: Int -> Int
countPresents n
    | result <= input = countPresents (n + 1)
    | otherwise = n
    where result = sum $ map (*10) $ filter (isDivisableBy n) [n,n-1..1]

isDivisableBy :: Int -> Int -> Bool
isDivisableBy x y = x `mod` y == 0