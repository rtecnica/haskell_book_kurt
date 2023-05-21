module Main where

import Data.Text.IO qualified as TIO
import Palindrome qualified

isPalindrome :: String -> Bool
isPalindrome text = text == (reverse text)

main :: IO ()
main = do
  print "Enter a word and I'll let you know if it's a palindrome!"
  text <- TIO.getLine
  let response =
        if Palindrome.isPalindrome text
          then "it is!"
          else "it's not!"
  print response
