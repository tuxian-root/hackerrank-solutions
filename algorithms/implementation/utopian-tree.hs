solve :: Int -> Int
solve n
    | n == 0 = 1
    | otherwise =
        if odd n
            then 2 * solve (n - 1)
            else 1 + solve (n - 1)

main = interact $ unlines . map show . map solve . map read . tail . words
