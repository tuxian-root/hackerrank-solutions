import Data.List (intersect)

intersectcount :: String -> String -> Int
intersectcount password criteriastring = length $ intersect password criteriastring

iscorrectlength :: Int -> Int -> Int
iscorrectlength lengthofpassword needed
    | needed + lengthofpassword >= 6 = needed
    | otherwise                      = 6 - lengthofpassword

solve :: [String] -> Int
solve [string] = iscorrectlength (length string)
               . length
               . filter ((==) 0)
               $ [intersectcount string "0123456789",
                  intersectcount string "abcdefghijklmnopqrstuvwxyz",
                  intersectcount string "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                  intersectcount string "!@#$%^&*()-+"]

main = interact $ show . solve . tail . words
