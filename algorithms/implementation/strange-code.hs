solve :: Int -> Int -> Int
solve n t
    | t <= n = n - t + 1
    | otherwise = solve (2 * n) (t - n)

main = interact $ show . solve 3 . read
