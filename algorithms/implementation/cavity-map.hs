sol :: [String] -> [String]
sol s = if n<3 then s else [[f i j | j <- [0..n-1]] | i <-[0..n-1]]
  where
  n = length s
  f i j = if 0<i && i<n-1 && 0<j && j<n-1 && s!!i!!j > g i j then 'X' else s!!i!!j
  g i j = maximum [s!!(i+1)!!j, s!!(i-1)!!j, s!!i!!(j+1), s!!i!!(j-1)]

main = interact $ unlines . sol . tail . words
