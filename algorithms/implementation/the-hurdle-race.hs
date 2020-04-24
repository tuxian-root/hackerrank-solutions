solve :: [Int] -> Int
solve (b:xs) = maximum $ 0 : (map (\a -> a - b) xs)

main = interact $ show . solve . map read . tail . words
