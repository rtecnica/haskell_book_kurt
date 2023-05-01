{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello", " ", name, "!"]

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson (T.pack name)
  TIO.putStrLn statement
