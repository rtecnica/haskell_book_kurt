main :: IO ()
main = do
  print "gimme a number"
  number <- getLine
  print number

-- TODO: Check how to print this shit

-- Q2.2
inc x = (\z -> (\y -> z + y) 1) x

double x = (\z -> (\y -> z * y) 2) x

square x = (\z -> (\y -> z ^ y) 2) x

-- Q2.2
scaleNum n =
  if modulo < 1
    then n - 2
    else 3 * n + 1
  where
    modulo = mod n 2
