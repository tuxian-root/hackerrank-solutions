main = interact $ show . length . filter (\(a,b) -> a /= b) . zip (cycle "SOS")
