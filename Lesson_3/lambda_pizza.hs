-- Q3.1
inc x = (\z -> (\y -> z + y) 1) x

double x = (\z -> (\y -> z * y) 2) x

square x = (\z -> (\y -> z ^ y) 2) x

counter x =
  let x = x + 1
   in let x = x + 1
       in x
