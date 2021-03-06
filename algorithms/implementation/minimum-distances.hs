import qualified Data.Map as Map

find_positions :: [(Int, Int)] -> Map.Map Int [Int]
find_positions ap = foldr (\x acc -> Map.insertWith (++) (fst x) ([snd x]) acc) Map.empty ap

find_min_dist :: [[Int]] -> Int -> Int
find_min_dist [] curr_dist = curr_dist
find_min_dist (pos : positions) curr_dist = find_min_dist positions curr_dist'
    where
        curr_dist'   = if length pos < 2 then curr_dist else min curr_dist (max_dist pos)
        max_dist pos = maximum pos - (minimum pos)

solve :: [Int] -> Int
solve (n:a) = if res < n then res else (-1)
    where ap        = zip a [0..n - 1]
          positions = Map.elems $ find_positions ap
          res       = find_min_dist positions n

main :: IO ()
main = interact $ show . solve . map read . words
