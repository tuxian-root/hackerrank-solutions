grade :: Int -> Int
grade v
    | v >= 38 && (mod5 - v) < 3  = mod5
    | otherwise                  = v
    where mod5 = v + (5 - v `mod` 5)

main = interact $ unlines . map show . solve . map read . tail . words
    where solve arr = map grade arr
