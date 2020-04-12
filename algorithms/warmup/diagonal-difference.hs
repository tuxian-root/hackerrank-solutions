import Control.Monad(replicateM)
import Data.List (transpose)

getIntSquareMatrix :: Int -> IO([[Int]])
getIntSquareMatrix rows = do
  matrix <- replicateM rows getLine
  let intMatrix = (map . map) read $ map words matrix
  return intMatrix

mainDiag :: [[a]] -> [a]
mainDiag x = zipWith (!!) x [0..]

secDiag :: [[c]] -> [c]
secDiag x = mainDiag $ transpose $ reverse  x

main = do
  number <- readLn :: IO Int
  matrix <- getIntSquareMatrix number
  putStrLn $ show $ abs $ foldr (-) 0 $ map sum [mainDiag matrix, secDiag matrix]
