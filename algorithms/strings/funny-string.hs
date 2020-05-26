import Data.Char (ord)
import Data.Function (on)

solve :: String -> String
solve =  ((\p-> if p then "Funny" else "Not Funny") .  and . (\s -> zipWith ((==) `on` abs) s (reverse s)) . (\s -> zipWith ((-)) (tail s) s) . map ord)

main = interact $ (unlines . map solve . tail . lines)
