import Data.List

swapPos d l r = take l d ++ [d !! r] ++ take (r - l - 1) (drop (l + 1) d) ++
  [d !! l] ++ drop (r + 1)  d

reversePos d l r = take l d ++ reverse (take (r - l + 1) (drop l d)) ++ drop (r + 1)  d

main = do
  getLine
  d <- fmap (map read . words) getLine :: IO [Int]
  let sortedD = sort d
  if d == sortedD
    then putStrLn "yes"
    else do
      let commonHead = takeWhile (\(a,b) -> a == b) $ zip d sortedD
      let commonTail = takeWhile (\(a,b) -> a == b) $ reverse $ zip d sortedD
      let l = length commonHead
      let r = length d - length commonTail - 1
      if swapPos d l r == sortedD
        then do
          putStrLn "yes"
          putStrLn $ "swap " ++ show (l + 1) ++ " " ++ show (r + 1)
        else if reversePos d l r == sortedD
          then do
            putStrLn "yes"
            putStrLn $ "reverse " ++ show (l + 1) ++ " " ++ show (r + 1)
          else
            putStrLn "no"
