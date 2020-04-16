solve :: Int -> Int -> [Int] -> [Int]
solve d m xs
    | null xs               = []
    | d == sum (take m xs)  = 1 : solve d m (tail xs)
    | otherwise             = solve d m (tail xs)

parseIn :: [Int] -> [Int]
parseIn (n:rest) = solve d m xs
    where d  = head $ drop n rest
          m  = last rest
          xs = take n rest

main :: IO ()
main = interact $ show . length . parseIn . map read . words
