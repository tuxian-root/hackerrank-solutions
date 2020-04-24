import Data.List
import Data.List.Split (chunksOf)

area h w xs = 2 * h * w + count xs + count (rotate xs)
  where rotate = reverse . transpose
        count = sum . map diff
        diff ys = sum $ zipWith ((abs .) . (-)) (0 : ys) (ys ++ [0])


solve :: [Int] -> Int
solve (h:w:xs) = area h w (chunksOf w $ xs)

main = interact $ show . solve . map read . words
