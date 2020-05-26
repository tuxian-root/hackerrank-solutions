import Data.List (group)
solve :: String -> Int
solve = sum . map (pred . length) . group

main = interact $ unlines . map ( show . solve) . tail . words
