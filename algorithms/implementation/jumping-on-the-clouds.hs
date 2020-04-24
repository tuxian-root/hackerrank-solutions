cloudJump :: [Int] -> Int
cloudJump [] = 0
cloudJump [x] = 0
cloudJump [x,y] = 1
cloudJump (x:y:z:xs)
    | z == 1 = 1 + cloudJump (y:z:xs)
    | otherwise = 1 + cloudJump (z:xs)

main :: IO ()
main = interact $ show . cloudJump . map read . tail . words
