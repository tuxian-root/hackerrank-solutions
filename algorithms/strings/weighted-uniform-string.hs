import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.List (group, elemIndex)

score :: String -> Set Int
score = S.fromList . concat . map (\x->[head x * i| i<-[1..length x]]) . group . map (\x-> fromMaybe 0 $ elemIndex x " abcdefghijklmnopqrstuvwxyz")

solve :: [String] -> [String]
solve (str:_:weights') = map (\x -> if (x `S.member` (score str)) then "Yes" else "No") $ weights
    where weights = map (read::String->Int) $ weights'

main = interact $ unlines . solve . words
