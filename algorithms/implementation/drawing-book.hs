solve :: [Int] -> Int
solve [totalpages, pagetogo] = min front back
    where front = pagetogo `div` 2
          back  = totalpages `div` 2 - front

main = interact $ show . solve . map (read :: String -> Int) . words
