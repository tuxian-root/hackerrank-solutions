isKeprekar :: Int -> Bool
isKeprekar 1 = True
isKeprekar n
    | n <= 3 = False
    | otherwise = (read low) + (read high) == n
    where
        sq = show $ n^2
        (low, high) = splitAt (length sq `div` 2) sq

solve :: [Int] -> String
solve (start:[end]) = (\xs -> if null xs then "INVALID RANGE" else (unwords $ map show xs))
                        $ filter isKeprekar
                        $ [start..end]

main = interact $ solve . map read . words
