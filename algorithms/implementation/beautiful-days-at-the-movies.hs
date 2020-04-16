isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

reverseInt :: Int -> Int
reverseInt x = (read :: String -> Int) $ reverse . show $ x

devide :: Int -> Int -> Float
devide a b = (fromIntegral a) / (fromIntegral b)

isBeautifulDay :: Int -> Int -> Float
isBeautifulDay k x = devide (abs (x - reverseInt x)) k

solve :: [Int] -> Int
solve (i:j:k:rest) = length $ filter (\a -> a) . map isInt . map (isBeautifulDay k) $ [i..j]

main = interact $ show . solve . map read . words
