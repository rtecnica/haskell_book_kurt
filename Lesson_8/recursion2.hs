-- Recursive length
myLength list =
  if null list
    then 0
    else myLength (drop 1 list) + 1

-- Q8.1
myReverse list =
  if null list
    then []
    else myReverse (drop 1 list) ++ [head list]

-- Q8.2
fastFib n1 n2 counter =
  case counter of
    0 -> 0
    1 -> 1
    n -> n1 + fastFib n2 (n1 + n2) (counter - 1)
