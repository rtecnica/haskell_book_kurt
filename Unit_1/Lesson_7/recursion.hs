-- Recursive GCD calculator by way of euclids method
recGCD a b =
  if remainder == 0
    then b
    else recGCD a remainder
  where
    remainder = a `mod` b

-- Conmutativity of operands
gcdcalc a b =
  case compare a b of
    GT -> recGCD a b
    LT -> recGCD b a
    _ -> b

-- QC7.3
myTail (_ : xs) = xs

-- Q7.1

myTail2 (x : xs) =
  if null x
    then []
    else xs

-- Recursive Take
myTake num list =
  if null list || num == 0
    then []
    else head list : myTake (num - 1) (tail list)

myDrop num (x : xs) =
  if null xs || num == 0
    then x : xs
    else myDrop (num - 1) (x : xs)
