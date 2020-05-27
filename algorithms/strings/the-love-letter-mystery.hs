import Data.Char (ord)

solve :: String -> Int
solve str = sum $ zipWith (\x y -> abs $ ord y - ord x) (take (length str `div` 2) str) (reverse str)

main = interact $ unlines . map (show . solve) . tail . lines
