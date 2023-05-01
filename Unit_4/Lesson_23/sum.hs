{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO
import Data.Text.Read qualified as TR

toInts :: T.Text -> [Int]
toInts txt = map (read . T.unpack) (T.lines txt)

main :: IO ()
main = do
  userInput <- TIO.getContents
  let numbers = toInts userInput
  print (sum numbers)
