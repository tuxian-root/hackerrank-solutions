import Data.Functor
import qualified Data.Matrix as M
import qualified Data.Set as S

readLine :: IO String
readLine =
    do line <- getLine
       return line

main :: IO ()
main = do
  [rows, cols] <- map read <$> words <$> readLine
  g            <- M.fromLists . lines <$> getContents
  let
    isValid (i, j)   = M.safeGet i j g == Just 'G'
    startFrom (i, j) = [(i, j)] : (tail $ iterate advance $ replicate 4 (i, j))
    advance [(a, b), (c, d), (e, f), (g, h)] = [(a+1, b), (c, d+1), (e-1, f), (g, h-1)]
    ps = concat
       $ [1..rows]
      <&> \i -> concat
       $ [1..cols]
      <&> \j ->
           map S.fromList
           . scanl1 (<>)
           . takeWhile (all isValid)
           $ startFrom (i, j)
  print $ maximum [S.size p * S.size q | p <- ps, q <- ps, p `S.disjoint` q]
