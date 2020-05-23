import Data.Char

encrypt :: [String] -> String
encrypt ( _ : str : [k']) = map (encryptChar k) str
    where k = read k'

encryptChar :: Int -> Char -> Char
encryptChar k c
    | isLower c = chr $ (+97) $ ((ord c) - (ord 'a') + k) `mod` 26
    | isUpper c = chr $ (+65) $ ((ord c) - (ord 'A') + k) `mod` 26
    | otherwise = c

main = interact $ encrypt . words
