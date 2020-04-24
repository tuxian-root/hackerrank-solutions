import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Data.List (inits, tails)

main :: IO ()
main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        _ <- getLine
        as <- map read . words <$> getLine :: IO [Int]
        -- Parity of permutations
        let x = map last . tail . inits $ as
        let xs = tail . tails $ as
        -- For each x find total number of values in following xs that are bigger than x
        -- And then count their sum.
        let invCount = sum $ zipWith (\h t -> length . filter ((>) h) $ t) x xs

        putStrLn $ if even invCount then "YES" else "NO"
