main = getContents >>= (\(w:t) -> mapM_(\[l,r] -> print . minimum . take(r-l+1) . drop l $ w) t) . map (map read . words) . tail . lines
