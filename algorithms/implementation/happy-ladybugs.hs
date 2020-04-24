import Control.Monad
import Data.List

happy :: [Char] -> Bool
happy b
    | '_' `elem` b = dynamic_happy b
    | otherwise    = static_happy b
    where static_happy  = all (\xs -> length xs > 1) . group
          dynamic_happy = static_happy . sort . filter (/= '_')

main :: IO ()
main = do
  g <- read <$> getLine
  replicateM_ g $ do
    b <- getLine >> getLine
    putStrLn $ if happy b then "YES" else "NO"
