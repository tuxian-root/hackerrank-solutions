import Data.List (nub)

solve :: String -> Int
solve ""  = 0
solve str = maximum (0:[length substr | a<-chars, b<-chars, a /= b, substr <- [filter (\c -> c==a || c==b) str], not $ doubles substr])
    where
        chars      = nub str
        doubles xs = any (uncurry (==)) $ zip xs (tail xs)

main = interact $ show . solve . last . words
