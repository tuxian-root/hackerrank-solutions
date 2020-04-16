import Data.List.Split (chunksOf)

solve :: [Int] -> Int
solve [p, q] = length $ takeWhile (<=q) $ dropWhile (<p) $ map (^2) [1..]

main = interact $ unlines . map show . map solve . chunksOf 2 . map read . tail . words
