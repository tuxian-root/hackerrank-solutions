import Data.Char (isUpper)

main = interact $ show . (+1) . length . filter isUpper
