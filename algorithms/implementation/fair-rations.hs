f :: Int -> [Int] -> Int
f c [] = c
f c [x] = c
f c l@(a:b:xs)
  | odd a = f (c+2) ((b+1):xs)
  | otherwise = f c (b:xs)

solve :: [Int] -> String
solve xs = if odd (sum xs) then "NO" else show (f 0 xs)

main = interact $ solve . map read . tail . words
