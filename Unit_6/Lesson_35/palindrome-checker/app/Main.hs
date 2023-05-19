-- {-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text.IO as TIO
import qualified Palindrome 

main :: IO ()
main = do
  TIO.putStrLn "Enter a word and I'll let you know if it's a palindrome!"
  text <- TIO.getLine
  let response =
        if Palindrome.isPalindrome text
          then "it is!"
          else "it's not!"
  TIO.putStrLn response
