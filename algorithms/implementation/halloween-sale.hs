solve :: [Int] -> Int
solve (p:d:m:[s]) = length . takeWhile (<=s) . scanl1 (+) $ [p, p-d .. m] ++ repeat m

main = interact $ show . solve . map read . words
