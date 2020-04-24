import Data.List
import Data.Function

solve :: [Int] -> Int
solve arr = arrl - length max
    where
        max  = head . sortBy (flip compare `on` length) . group . sort $ arr
        arrl = length arr

main = interact $ show . solve . map read . tail . words
