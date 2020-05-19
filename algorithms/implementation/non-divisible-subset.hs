-- https://www.hackerrank.com/rest/contests/master/challenges/non-divisible-subset/hackers/jhrcek/download_solution

import Control.Arrow
import Data.List (sort, group)

main :: IO ()
main = do
    [_n,k] <- readInts
    ns     <- readInts
    let rs = map (head &&& length) . group . sort $ fmap (`mod`k) ns
    print . sum $ map (pick rs k) [0..div k 2]

pick :: [(Int, Int)] -> Int -> Int -> Int
pick xs k r = case (lookup r xs,lookup (k-r) xs) of
    (Just x,  Just y)  -> if r == (k-r) then 1 else max x y
    (Just x,  Nothing) -> if r == 0     then 1 else x
    (Nothing, Just y)  -> y
    (Nothing, Nothing) -> 0

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

--| r -> remainder
--| k -> divisor
--| elements in the array are quotient

------------------------------------------------------------------------------------------

{-| Another Approach
module Main where

import qualified Data.Map as Map
import qualified Data.Vector as Vec
counter :: [Int] -> Int -> Map.Map Int Int
counter a k = foldr (\key acc -> Map.insertWith (+) (key `mod` k) 1 acc) (Map.fromList [(x, 0) | x <- [0..k - 1]]) a

solve pos freq k r
    | pos == k `div` 2 + 1 = r
    | otherwise            = solve (pos + 1) freq k r'
    where
        r' = max (freq Vec.! pos) (freq Vec.! (k - pos)) + r

main :: IO ()
main = do
    param <- getLine
    a_tmp <- getLine
    let [n, k] = map (read :: String -> Int) $ words param
        a = map (read :: String -> Int) $ words a_tmp
        c = Map.elems $ counter a k
        r = if c !! 0 > 0 then 1 else 0
        h = k `div` 2
        res' = solve 1 (Vec.fromList c) k r
        res = if (even k) && (c !! h > 1) then res' - (c !! h - 1) else res'
    print res
-}
------------------------------------------------------------------------------------------
{-| Another Approach
import Data.Array
import Data.Maybe
import qualified Data.ByteString.Char8 as B

main = do
  [n, k] <- (map read . words) <$> getLine
  a <- (accumArray (+) 0 (0, k-1) . map ((\x -> (x `rem` k, 1)) . fst . fromJust . B.readInt) . B.words) <$> B.getLine :: IO (Array Int Int)
  print $ sum [if (2*i) `rem` k == 0 then min (a!i) 1 else max (a!i) (a!(k-i)) | i <- [0..k `quot` 2]]
-}
