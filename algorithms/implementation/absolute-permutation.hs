import Control.Monad
import Data.List

solve :: Int -> Int -> [Int]
solve n 0 = [1..n]
solve n k
  | r==0      = concatMap fun [1,3..2*q]
  | otherwise = [-1]
  where
    (q,r) = n `divMod` (2*k)
    fun i = [i*k+1..(i+1)*k] ++ [(i-1)*k+1..i*k]

readLine :: IO String
readLine =
    do line <- getLine
       return line

main :: IO ()
main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \a  -> do
        [n, k] <- map read <$> words <$> readLine
        putStrLn . unwords . map show $ solve n k
