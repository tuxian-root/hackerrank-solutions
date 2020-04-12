main = interact $ unlines . map show . f . map read . tail . words
       where f xs = map (g xs . ($ xs)) [filter (> 0), filter (< 0), filter (== 0)]
             g a b = fromIntegral (length b) / fromIntegral (length a)
