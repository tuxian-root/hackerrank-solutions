-- https://coding-gym.org/challenges/append-and-delete/
appendAndDelete :: [String] -> Bool
appendAndDelete (s:t:k1) = canRemAddLast || canRemAll
    where
        commonLen = length $ takeWhile (uncurry (==)) (zip s t)
        sLen = length s
        tLen = length t
        diff = (sLen + tLen - (2 * commonLen))
        canRemAddLast = diff <= k && (even $ k - diff)
        canRemAll = sLen + tLen < k
        [k] = map (read :: String -> Int) $ k1

main = interact $ (\x -> if (x == True) then "Yes" else "No" ) . appendAndDelete . words
