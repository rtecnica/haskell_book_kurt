import Data.Map qualified as Map

-- Q21.1
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

nameData :: Map.Map Int String
nameData = Map.fromList [(1, "Powell"), (2, "Argyle")]

mainMaybe :: Maybe String
mainMaybe = do
  firstName <- Map.lookup 1 nameData
  return firstName

-- Q21.2
fastFib :: Int -> Int -> Int -> Int
fastFib n1 n2 counter =
  case counter of
    0 -> 0
    1 -> 1
    n -> n1 + fastFib n2 (n1 + n2) (counter - 1)

fib :: Int -> Int
fib n = fastFib 0 1 n

main :: IO ()
main = do
  putStrLn "Pleas input number:"
  num <- getLine
  let fibnum = read num
  print (fib fibnum)
