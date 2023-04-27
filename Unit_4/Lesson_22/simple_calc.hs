import Data.Text (splitOn, pack, unpack, strip)
-- Q22.1
-- Write a program, simple_calc.hs, that reads simple equations involving adding
-- two numbers or multiplying two numbers. The program should solve the equation each
-- user types into each line as each line is entered.

main :: IO ()
main = do
  userInput <- getContents
  print (printExpr (parsExpr userInput))

data Expr = Expr
  { args :: (Int, Int),
    operator :: Operator
  }

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && not (value1 && value2)

evalExpr :: Expr -> Int
evalExpr (Expr args operator) = case operator of
                                  Sum -> firstarg + secondarg
                                  Prod -> firstarg * secondarg
                                  where firstarg = fst args
                                        secondarg = snd args

printExpr :: Maybe Expr -> String
printExpr (Just expr) = show (evalExpr expr)
printExpr Nothing = "Invalid Expression"

data Operator = Sum | Prod deriving (Show, Eq)

checkExprType :: String -> Maybe Operator
checkExprType expr = if xorBool is_sum is_prod
                      then if is_sum
                        then Just Sum
                        else Just Prod
                      else Nothing
                    where is_sum = '+' `elem` expr
                          is_prod = '*' `elem` expr

parsExpr :: String -> Maybe Expr
parsExpr expr =  case exprType of
                    Just Sum -> if length splitsum == 2
                            then Just (Expr parsedsum Sum)
                            else Nothing
                    Just Prod -> if length splitprod == 2
                            then Just (Expr parsedprod Prod)
                            else Nothing
                    Nothing -> Nothing
                  where exprType = checkExprType expr
                        splitsum = splitOn (pack "+") (pack expr)
                        splitprod = splitOn (pack "*") (pack expr)
                        parsedsum = tuplefy (map (read . unpack) splitsum)
                        parsedprod = tuplefy (map (read . unpack) splitprod)

tuplefy :: [a] -> (a, a)
tuplefy [x, y] = (x, y)