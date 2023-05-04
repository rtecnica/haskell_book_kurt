-- Q25.1 Write a program that reads in a text file and outputs the difference between the
-- number of characters in the file and the number of bytes in the file.
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import System.Environment
import System.Random

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  bytesFile <- BC.readFile fileName
  charsFile <- readFile fileName
  let charLength = length charsFile
  let bytesLength = BC.length bytesFile
  let diff = bytesLength - charLength
  print ("#Chars: " ++ (show charLength) ++ " - #Bytes: " ++ (show bytesLength) ++ " - Diff: " ++ (show diff))
