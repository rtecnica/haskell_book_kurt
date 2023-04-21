-- Q13.3
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if n /= maxBound
    then succ n
    else minBound