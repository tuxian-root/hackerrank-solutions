import Data.List (intersect)

main = interact $ show . length . foldr1 intersect . tail . words
