import Data.List (sort)

solve :: [Int] -> [Int]
solve [] = []
solve numbers@(x:xs) =
    let l          = length numbers
        newNumbers = dropEqual x xs
        in (l : solve newNumbers)
    where dropEqual _ [] = []
          dropEqual y ls@(z:zs)
              | z == y    = dropEqual y zs
              | otherwise = ls

main = interact $ unlines . map show . solve . sort . map read . tail . words
