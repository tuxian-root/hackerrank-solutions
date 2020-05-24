import Data.Char (toLower)

solve :: String -> String
solve str = let go = all (`elem` str) ['a'..'z'] in
    case go of
        True  -> "pangram"
        False -> "not pangram"

main = interact $ solve . map toLower
