import qualified Data.Matrix as Matrix
import Control.Monad

contains mgrid mpattern rg cg rp cp = do
    any (==True) [Matrix.submatrix r (r + rp - 1) c (c + cp - 1) mgrid ==  mpattern | r <- [1..(rg - rp + 1)], c <- [1..(cg - cp + 1)]]

readLine :: IO String
readLine =
    do line <- getLine
       return line

main = do
    t <- readLn :: IO Int
    forM [1..t] $ \_ -> do
        [gr, gc] <- map read <$> words <$> readLine
        grid     <- replicateM gr readLine
        [pr, pc] <- map read <$> words <$> readLine
        pattern  <- replicateM pr readLine
        let ans = contains (Matrix.fromLists grid) (Matrix.fromLists pattern) gr gc pr pc
        if ans
            then putStrLn "YES"
            else putStrLn "NO"
