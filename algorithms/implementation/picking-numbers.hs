import Data.List

main :: IO ()
main =  interact $ show . solve . map read . tail . words

solve :: [Int] -> Int
solve = maximum . map length . groupBy (\a b -> abs (a-b) <= 1) . sort
