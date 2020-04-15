import Data.List

main = interact $ show . solve . last . words
    where solve =   length .
                    filter (all (< 0)) .
                    groupBy (\x y -> x /= 0 && y /= 0) .
                    scanl (+) 0 .
                    map (\c -> if c == 'U' then 1 else -1)
