import Control.Monad ( liftM2 )
import Data.List ( sortBy )
import Data.Maybe ( listToMaybe, fromMaybe )

solve :: [Int] -> Int
solve (b:n:_:rest) = fromMaybe (-1) . listToMaybe . sortBy (flip compare) . filter ( <= b ) $ [ x + y | x <- k, y <- m ]
           where k = take n rest
                 m = drop n rest

main = interact $ show . solve . map read . words
