import Data.List (zipWith)
import Control.Monad (replicateM)

allSqrMat :: [[Int]]
allSqrMat = [[a, b, 15-a-b, 20-a-a-b, 5, a+a+b-10, a+b-5, 10-b, 10-a ] | a<-[2,4,6,8], b<-[1,3,7,9], a+b >= 7, a+b <=13, a+a+b/=15]

trans :: [Int] -> [Int] -> Int
trans xs ys = sum $ map abs $ zipWith (-) xs ys

main :: IO ()
main = interact $ show . minimum . (\xs -> map (trans xs) allSqrMat) . map read . words
