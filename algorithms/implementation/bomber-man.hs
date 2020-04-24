import Data.Matrix

bomb = 'O'
empty = '.'

bomberman :: Matrix Char -> Int -> Matrix Char
bomberman m n
    | n == 1 = m
    | even n = const bomb <$> m
    | odd (div n 2) = mBoom m
    | otherwise = mBoom $ mBoom m
    where mBoom _m = matrix (nrows _m) (ncols _m) (boom _m)

boom :: Matrix Char -> (Int, Int) -> Char
boom m (i,j)
    | any (==bomb)
        . map (m!)
        . filter (\(i,j) -> elem i [1..nrows m] && elem j [1..ncols m])
        $ [(i,j), (i-1,j), (i+1,j), (i,j-1), (i,j+1)]
        = empty
    | otherwise = bomb

main = do
    [_,_,n] <- map read . words <$> getLine
    m <- fromLists . lines <$> getContents
    mapM_ putStrLn $ toLists $ bomberman m n
