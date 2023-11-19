import Data.List (iterate')

row = 3010
column = 3019

first = 20151125
mulBy = 252533
divBy = 33554393

main = print getCode

getCode :: Integer
getCode = iterate' (\n -> n * mulBy `mod` divBy) first !! calculateSequencePos

calculateSequencePos :: Int
calculateSequencePos = seqVal - 1
    where colVal = scanl (+) 0 [1..] !! column
          seqVal = scanl (+) colVal [column..] !! (row - 1)
