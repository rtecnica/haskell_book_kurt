-- Q32.1 Use a list comprehension that generates a list of correct calendar dates, given
-- that you know the number of days in each month. For example, it should start with 1 ..
-- 31 for January and be followed by 1 .. 28 for February.

datesYear :: [Int]
datesYear = [day | month <- [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
                day <- [1..month]]

-- Q32.2 Translate the preceding question into do-notation, and then into Monad methods
-- and lambdas.

datesYearDoNotation :: [Int]
datesYearDoNotation = do
    month <- [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    day <- [1..month]
    return day

datesYearMonad :: [Int]
datesYearMonad = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] >>= (\n -> [1..n])