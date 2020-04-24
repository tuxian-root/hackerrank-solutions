import Data.List
import Data.Function

solve :: [Int] -> [Int]
solve xs = map snd $ sortBy (compare `on` fst) $ zip xs $ map fst $ sortBy (compare `on` snd) $ zip [1..length xs] xs

main = interact $ unlines . map show . solve . map read . tail . words
