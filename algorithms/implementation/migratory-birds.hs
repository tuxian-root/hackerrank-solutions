import Data.List (group, sort, sortBy)
import Data.Function (on)

main = interact $ show . head . head . sortBy (flip compare `on` length) . group . sort . map (read::String->Int) . tail . words
