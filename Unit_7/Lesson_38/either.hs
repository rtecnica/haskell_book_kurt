import qualified Data.Char as C
import Data.Maybe

-- Q38.1
data ParseErr = FirstErr | SecondErr | BothErr

addStrInts :: String -> String -> Either String Int
addStrInts n1 n2 = case (firstParsable, secondParsable) of
    (True, True) -> Right (firstNum + secondNum)
    (False, True) -> Left "First Arg Unparsable"
    (True, False) -> Left "Second Arg Unparsable"
    (False, False) -> Left "Both Arg Uparsable"
    where firstParsable = all C.isDigit n1
          secondParsable = all C.isDigit n2
          firstNum = read n1
          secondNum = read n2


-- Q38.2

mySucc :: (Bounded a, Eq a, Enum a) => a -> Maybe a
mySucc enum = if enum == maxBound
    then Nothing
    else Just (succ enum)

myTail :: [a] -> [a]
myTail [] = []
myTail list = tail list

listUpperBound :: Int
listUpperBound = 10000

myLength :: Int -> [a] -> Maybe Int
myLength _ [] = Just 0
myLength n (x:xs) = if n < listUpperBound
    then myLength (n + 1) xs
    else Nothing

myLast :: [a] -> Either String a
myLast [] = Left "Empty list!"
myLast list = if isNothing (myLength 0 list)
    then Left "List is too long!"
    else Right (last list)