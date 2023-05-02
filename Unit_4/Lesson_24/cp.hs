-- Q24.1

import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let firstArg = head args
  let secondArg = args !! 1
  source <- readFile firstArg
  writeFile secondArg source
