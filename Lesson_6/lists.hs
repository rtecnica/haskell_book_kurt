-- Q6.1
repeat val = cycle [val]

-- Q6.2
subseq start end list = take (end - start) (drop start list)

-- Q6.3
inFirstHalf item list = elem item (take ((length list) `div` 2) list)
