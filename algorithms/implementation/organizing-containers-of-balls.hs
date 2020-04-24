-- https://www.hackerrank.com/rest/contests/master/challenges/organizing-containers-of-balls/hackers/jhrcek/download_solution

import Control.Monad
import Data.Bool
import Data.List

main :: IO ()
main = do
  q <- readLn
  replicateM_ q $ do
    n <- readLn
    matrix <- replicateM n getInts
    putStrLn . bool "Impossible" "Possible" $ sort (map sum matrix) == sort (map sum $ transpose matrix)

getInts :: IO [Int]
getInts = fmap (fmap read . words) getLine
