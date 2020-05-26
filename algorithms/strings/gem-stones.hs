import Data.List (intersect, nub)

main = interact $ show . length . nub . foldr1 intersect . tail . words
