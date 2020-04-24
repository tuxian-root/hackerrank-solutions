fromWrapper m n | m > n = 0
                | otherwise = (div n m) + fromWrapper m ((div n m) + (mod n m))

totalCandies [] = 0
totalCandies (n:c:m:xs) =  (div n c) + (fromWrapper m (div n c))

main :: IO ()
main = interact $ unlines . map show . map totalCandies. map (map read). map words. tail. lines
