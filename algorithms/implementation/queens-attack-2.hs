import qualified Data.Set as Set
import Data.Ix (inRange)

solve::[[Int]] -> Int
solve ([n,k]:[r,c]:rest) = sum.map (move 0 (r,c)) $ [(1,0),(0,1),(-1,0),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]
    where step (dx,dy) (x,y) = (x+dx,y+dy)
          move n pos dir = let next = step dir pos in if isGoodDirection next then move (n+1) next dir else n
          obstaclesSet = Set.fromList.map (\[a,b] -> (a,b)) $ rest
          isGoodDirection :: (Int,Int) -> Bool
          isGoodDirection pos = (inRange ((1,1),(n,n)) pos) && (Set.notMember pos obstaclesSet)

main = interact $ show . solve . map (map read.words) . lines
