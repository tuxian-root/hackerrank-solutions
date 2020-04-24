bigger :: String -> String
bigger = go [] . reverse
  where go xs (x:y:ys)
          | x <= y    = go (x:xs) (y:ys)
          | otherwise = reverse (w:ys) ++ vs ++ (y:ws)
          where
            (vs,w:ws) = break (>y) $ reverse (x:xs)
        go _ _ = "no answer"

main = interact $ unlines . map bigger . tail . words
