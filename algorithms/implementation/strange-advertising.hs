solve :: [Int] -> Int -> Int
solve arr n
    | n > 1     = solve (liked:arr) (n-1)
    | otherwise = sum arr
    where liked = (head arr * 3) `div` 2

main = interact $ show . solve [2] . read
