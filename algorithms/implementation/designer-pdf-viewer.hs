main = do
    size <- fmap (zip ['a'..'z'] . map read . words) getLine
    word <- getLine
    print $ length word * (maximum . map snd . filter (flip elem word . fst) $ size)
