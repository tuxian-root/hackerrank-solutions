import Data.List (group, sort)

main = interact $ show . sum . map (flip div 2) . map length . group . sort . map (read::String->Int) . tail . words
