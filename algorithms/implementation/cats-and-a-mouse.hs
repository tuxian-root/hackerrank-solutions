import Data.List.Split (chunksOf)

solve :: [Int] -> String
solve input
    | catA == catB = "Mouse C"
    | catA > catB  = "Cat B"
    | otherwise    = "Cat A"
    where catA = abs $ input !! 0 - input !! 2
          catB = abs $ input !! 1 - input !! 2

main = interact $ unlines . map solve . chunksOf 3 . map (read::String->Int) . tail . words
