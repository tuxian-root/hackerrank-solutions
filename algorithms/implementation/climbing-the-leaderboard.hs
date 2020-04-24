import Data.List(map, words)
import Data.Set(size, findIndex, insert, fromDescList)

climbingLeaderboard scores alice = map rank alice
    where rank a = let scores' = insert a scores in size scores' - findIndex a scores'

main = do
    getLine
    scores <- fromDescList . map read . words <$> getLine
    getLine
    alice <- map read . words <$> getLine :: IO [Int]
    mapM_ print $ climbingLeaderboard scores alice
