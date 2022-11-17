import Text.Printf (IsChar (toChar))

main :: IO ()
main = do
  print "gimme a number"
  number <- getLine
  print number

-- TODO: Check how to print this shit

-- Q2.2
inc x = x + 1

double x = x * 2

square x = x ^ 2

-- Q2.2
scaleNum n =
  if modulo < 1
    then n - 2
    else 3 * n + 1
  where
    modulo = mod n 2
