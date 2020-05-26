import Data.List (isPrefixOf)

yesorno 0 = "NO"
yesorno i = "YES " ++ show i

extractnumbers ('0':_) = 0
extractnumbers s       = headorzero $ filter (validnumber s) si
    where si = [read (take n s) | n <- [1..div (length s) 2]]

headorzero [] = 0
headorzero ii = head ii

validnumber "" _ = True
validnumber s  i = let si = show i in isPrefixOf si s && validnumber (drop (length si) s) (i+1)

main = interact $ unlines . map yesorno . map extractnumbers . tail . words
