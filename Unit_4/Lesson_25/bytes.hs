import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import GHC.Float (timesDouble)
import System.Environment
import System.Random (randomRIO)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitchedByte <- randomSortSection imageFile 5
  glitched <- randomReplaceByte glitchedByte 25
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"

intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where
    safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    newChar = intToBC charVal

foldL f (x : xs) = foldl f x xs

randomReplaceByte :: BC.ByteString -> Int -> IO BC.ByteString
randomReplaceByte bytes times = do
  let bytesLength = BC.length bytes
  location <- replicateM times (randomRIO (1, bytesLength))
  charVal <- replicateM times (randomRIO (0, 255))
  let funcs = zipWith replaceByte location charVal
  let final_func = foldL (.) funcs
  return (final_func bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> Int -> IO BC.ByteString
randomSortSection bytes times = do
  sectionSize <- replicateM times (randomRIO (10, 25))
  let bytesLength = BC.length bytes
  start <- mapM (randomRIO . (\x -> (1, bytesLength - x))) sectionSize
  let funcs = zipWith sortSection start sectionSize
  let final_func = foldL (.) funcs
  return (final_func bytes)
