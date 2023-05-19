module Palindrome (isPalindrome) where

import Data.Char (isPunctuation, isSpace, toLower)
import qualified Data.Text as T

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace text = T.pack (filter (not . isSpace) unpackedtext)
  where
    unpackedtext = T.unpack text

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.pack (filter (not . isPunctuation) unpackedtext)
  where
    unpackedtext = T.unpack text

toLowerCase :: T.Text -> T.Text
toLowerCase text = T.pack (map toLower unpackedtext)
  where
    unpackedtext = T.unpack text

preProcess :: T.Text -> T.Text
preProcess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where
    cleanText = preProcess text
