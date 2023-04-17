import Data.Char (toLower)

-- Q9.1 Use 'filter' and 'length' to recreate elem
myElem item list =
  length (filter (item ==) list) >= 1

-- Q9.2
isPalindrome candidate =
  processed_candidate == reverse processed_candidate
  where
    processed_candidate = map toLower (filter (' ' /=) candidate)

-- Q9.3
harmonic n =
  foldl (+) 0 (map (1 /) [1 .. n])
