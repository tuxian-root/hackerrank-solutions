import Control.Monad

solve b w x y z
    | x + z < y = x * (b + w) + z * w
    | y + z < x = y * (b + w) + z * b
    | otherwise = x * b + y * w

readIntList :: IO [Int]
readIntList =
    do line <- getLine
       return $ map read $ words line

main :: IO ()
main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \_  -> do
        [b, w]    <- readIntList
        [x, y, z] <- readIntList
        print $ solve b w x y z
