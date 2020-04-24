import Data.List (sort, nub)
import Data.List.Split (chunksOf)

solve :: [Int] -> [Int]
solve (n:a:[b]) = nub $ sort $ [a * i + b * (n - 1 - i) | i <- [0..n - 1]]

main = interact $ unlines . map unwords . map (map show) . map solve . chunksOf 3 . map (read::String->Int) . tail . words
