import Data.List (group)

solve :: String -> String
solve str = (iterate reducestring str) !! (length str)

reducestring :: String -> String
reducestring str = foldl (\state s -> if length s `mod` 2 == 0 then state else (state ++ [head s])) [] (group str)

emptyorstr :: String -> String
emptyorstr []  = "Empty String"
emptyorstr str = str

main = interact $ emptyorstr . solve
