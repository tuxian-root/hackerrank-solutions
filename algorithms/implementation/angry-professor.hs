import System.IO
import Control.Monad

solve :: [Int] -> String
solve (k:arr)
    | len < k   = "YES"
    | otherwise = "NO"
    where
        len = length $ filter (<=0) arr

parseIn :: IO [[Int]]
parseIn = do
    t <- readLn :: IO Int
    forM [1..t] $ \_ -> do
        [n,k] <- map read <$> words <$> getLine
        arr   <- map read <$> words <$> getLine
        return (k:arr)

main =  do
    res <- map solve <$> parseIn
    mapM putStrLn res
