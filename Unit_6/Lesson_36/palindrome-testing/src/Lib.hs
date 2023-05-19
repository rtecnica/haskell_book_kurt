module Lib
    ( isPalindrome
    ) where

isPalindrome :: String -> Bool
isPalindrome text = text == reverse text