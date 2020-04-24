digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = n `mod` 10 : digits (n `div` 10)

solve :: Int -> Int
solve n = length $ filter (\x -> x > 0 && n `mod` x == 0) (digits n)

main = interact $ unlines . map show . map solve . map read . tail . words
