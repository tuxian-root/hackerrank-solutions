circularArrayRotation :: [Int] -> Int -> [Int] -> [Int]
circularArrayRotation a k = fmap go
    where
        go q = a !! mod (q - k) n
        n = length a

main = do
    [_, k, _] <- fmap read . words <$> getLine
    a <- fmap read . words <$> getLine
    qs <- fmap read . lines <$> getContents
    putStr . unlines $ show <$> circularArrayRotation a k qs
