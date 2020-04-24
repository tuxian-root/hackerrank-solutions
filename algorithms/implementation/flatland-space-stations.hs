import Data.List

solve :: [Int] -> Int
solve (n:m:arr) = maximum (a:b:ds)
    where a  = head xs
          b  = n-1 - (head (reverse xs))
          xs = sort arr
          ds = zipWith (\a b -> div (a-b) 2 ) (tail xs) xs

main = interact $ show . solve . map read . words
