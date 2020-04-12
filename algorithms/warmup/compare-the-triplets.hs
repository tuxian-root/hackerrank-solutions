pair :: [Int] -> [(Int, Int)]
pair xs =
    let (a, b) = splitAt 3 xs in zip a b

compPoints :: [Int] -> (Int, Int) -> [Int]
compPoints [at, bt] (a, b)
    | a > b = [succ at, bt]
    | a < b = [at, succ bt]
    | otherwise = [at, bt]

main = interact $ unwords . map  show . foldl compPoints [0, 0] . pair . map read . words
