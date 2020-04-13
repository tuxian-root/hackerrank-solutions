import Data.List.Split (splitOn)
import Data.List (intercalate)
import Text.Printf (printf)

time24 time@(h:rest) format
  | format == "AM" && h == 12 = 0 : rest
  | format == "PM" && h == 12 = time
  | format == "PM" = h + 12 : rest
  | format == "AM" = time

main :: IO ()
main = do s <- getLine
          let time = map (read :: String -> Int) $ splitOn ":" $ take 8 s
          let format = drop 8 s
          putStrLn $ intercalate ":" $ map (printf "%02d") $ time24 time format

-- Reference: https://github.com/ryukinix/haskell-rank/blob/master/TimeConversion.hs
