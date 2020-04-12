sumdiv :: Int -> [Int] -> Int
sumdiv k []     = 0
sumdiv k (x:xs) = (sumdiv k xs) + length divs
    where divs  = filter (\y -> (x + y) `mod` k == 0) xs

solve :: [Int] -> Int
solve (_:k:arr) = sumdiv k arr

main = interact $ show . solve . map read . words
