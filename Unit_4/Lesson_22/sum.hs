main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum (map (^ 2) numbers))

toInts :: String -> [Int]
toInts = map read . lines
