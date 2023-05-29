import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bifunctor (bimap)
import Data.Array.Base (UArray(UArray))
import Control.Arrow (Arrow(arr))

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals - 1
  stArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray stArray i val
  return stArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = (snd . bounds) myArray
  forM_ [1 .. end] $ \i -> do
    forM_ [0 .. (end - i)] $ \j -> do
      val <- readArray stArray j
      nextVal <- readArray stArray (j + 1)
      let outOfOrder = val > nextVal
      when outOfOrder $ do
        writeArray stArray j nextVal
        writeArray stArray (j + 1) val
  return stArray

-- Q42.1 One of the most important operations in the implementation of a genetic algo-
-- rithm is combining two arrays of Booleans through an operation called crossover. Cross-
-- over takes as input a pair of equal-sized arrays. Then a cutoff point is chosen, and the
-- top and bottom are swapped. The final value is this new pair of arrays. Here’s an illus-
-- tration using lists and an example (using 1 for True and 0 for False):
--        ([1,1,1,1,1],[0,0,0,0,0])
-- If you perform crossover at index 3, your result should be
--        [1,1,1,0,0]
-- Implement crossover where the result is a UArray but the crossover itself is performed
-- using STUArrays.

crossOver :: UArray Int Bool -> UArray Int Bool -> Int -> UArray Int Bool
crossOver arr1 arr2 cutoff = runSTUArray $ do 
  stArr1 <- thaw arr1 
  let end1 = (snd . bounds) arr1
  forM_ [cutoff .. end1] $ \i -> do
    let val1 = arr2 ! i
    writeArray stArr1 i val1
  return stArr1

-- Q42.2 Write a function that takes a UArray Int Int as an input. The input will have a
-- mixture of zeros and other values. The function, replaceZeros, should return the array
-- with all of the zeros replaced with the value –1.

replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros arr = runSTUArray $ do
  stArr <- thaw arr
  let end = (snd . bounds) arr
  let bgn = (fst . bounds) arr
  forM_ [bgn .. end] $ \i -> do
    let item = arr ! i  
    when (item == 0) $ do
      writeArray stArr i (-1)
  return stArr