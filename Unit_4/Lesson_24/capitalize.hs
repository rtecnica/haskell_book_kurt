-- Q24.2

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  source <- TIO.readFile fileName
  TIO.writeFile fileName (T.toUpper source)
