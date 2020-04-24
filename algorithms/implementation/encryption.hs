encrypt :: Int -> Int -> String -> [String]
encrypt row col message = [ [ message !! j | j <- [i, i+col..(length message)-1] ] | i <- [0..col-1] ]

parseInAndEncrypt :: String -> String
parseInAndEncrypt message = unwords $ encrypt row col message
    where row = floor   $ sqrt $ fromIntegral $ length message
          col = ceiling $ sqrt $ fromIntegral $ length message

main = do
    input <- getLine
    putStrLn $ parseInAndEncrypt input
