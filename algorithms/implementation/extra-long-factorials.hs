main = interact $ show . fact . read
    where fact n = last $ take (n+1) $ scanl (*) 1 [1..]
