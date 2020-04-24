findMaxOccurrences :: [Int] -> Int
findMaxOccurrences xs = length $ filter (==maximum xs) $ xs

numTopics ([], []) = 0
numTopics ((x:team1), (y:team2))
    | x == '1' || y == '1' = 1 + numTopics (team1, team2)
    | otherwise            =     numTopics (team1, team2)

pairwiseTopics :: [String] -> [Int]
pairwiseTopics (n':m':topics) = map (numTopics) [(topics !! i, topics !! j) | i <- [0..(n - 1)], j <- [(i + 1)..(n - 1)]]
    where n = read n' :: Int
          m = read m' :: Int

main = interact $ unlines . map show . solve . pairwiseTopics . words
    where solve :: [Int] -> [Int]
          solve xs = [maximum xs] ++ [findMaxOccurrences xs]
