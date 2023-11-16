import Data.List (sort)
import Data.List.Split (splitOn)

main = do
    input <- readFile "inputs/2.txt"
    print $ sum $ map calculateSize $ lines input

calculateSize :: String -> Int
calculateSize s = 2*l*w + 2*w*h + 2*h*l + smallestSide
    where [l, w, h] = map read $ splitOn "x" s
          smallestSide = product $ init $ sort [l, w, h]
