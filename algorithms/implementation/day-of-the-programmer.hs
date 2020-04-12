isLeapYear :: Int -> Bool
isLeapYear year
  | year `rem` 400 == 0 = True
  | year `rem` 100 == 0 = False
  | year `rem`   4 == 0 = True
  | otherwise           = False

dayOfTheProgrammer :: Int -> String
dayOfTheProgrammer year
        | (year < 1918 && year `rem` 4 == 0) = "12.09." ++ show year
        | (year > 1918 && isLeapYear year)   = "12.09." ++ show year
        | (year == 1918)                     = "26.09." ++ show year
        | otherwise                          = "13.09." ++ show year

main = interact $ dayOfTheProgrammer . read
