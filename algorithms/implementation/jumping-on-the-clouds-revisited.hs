jumpClouds :: [Int] -> Int
jumpClouds (k:arr) = abs . subtract 100 . sum . map (calcEnergy arr)
                        $ (\xs-> xs ++ [0]) -- appending with 0 because we need to come back to same cloud where we started
                        $ takeWhile (>0)
                        $ [(i*k) `mod` (length arr) | i <- [1..(length arr)-1]]

calcEnergy :: [Int] -> Int -> Int
calcEnergy arr cloudInd = if arr !! cloudInd == 1 then 3 else 1

main = interact $ show . jumpClouds . tail . map read . words
