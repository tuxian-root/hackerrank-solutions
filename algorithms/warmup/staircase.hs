main = interact $ f . read
    where   f n = concat [replicate (n-i) ' ' ++ replicate i '#' ++ "\n" | i <- [1..n]]
