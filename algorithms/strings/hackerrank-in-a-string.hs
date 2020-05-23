solve [] _ = "YES"
solve _ [] = "NO"
solve l@(x:xs) (y:ys)
    | x == y    = solve xs ys
    | otherwise = solve l ys

main = interact $ unlines . map (solve "hackerrank") . tail . lines
