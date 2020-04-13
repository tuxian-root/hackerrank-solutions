parseInputsAndSolve :: [Int] -> String
parseInputsAndSolve xs
    | total == tsum     = "Bon Appetit"
    | otherwise         = show $ total - tsum
    where tsum = (sum list - list !! k) `div` 2
          (n:k:rest)    = xs
          list          = take n rest
          total         = sum $ drop n rest

main = interact $ parseInputsAndSolve . map read . words
