import Data.List.Split (chunksOf)

solve :: [Int] -> Int
solve (_:k:ts) =
    length . filter (\(a, b) -> b `elem` a) $ zip (concatMap (chunksOf k) [ [1..n] | n <- ts ]) [1..]

main = interact $ show . solve . map read . words
