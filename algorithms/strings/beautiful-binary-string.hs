import Data.Text (pack, count)

main = interact $ show . count (pack "010") . pack . last . words
