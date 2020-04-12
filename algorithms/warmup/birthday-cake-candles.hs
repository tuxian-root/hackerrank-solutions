main = interact $ show . f . map read . tail . words
        where f :: [Int] -> Int
              f arr = (length . filter (== maximum arr)) arr
