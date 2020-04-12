main = interact $ unwords . map show . f . map read . words
  where f xs = let [ma, mi, su] = sequenceA [maximum, minimum, sum] xs
               in  [su - ma, su - mi]
