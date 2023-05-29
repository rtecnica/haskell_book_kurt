import Data.Array.Unboxed

beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) $ zip [0 .. 3] $ cycle [0] 

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1,5), (3,6)]

turboBiB :: UArray Int Int
turboBiB = beansInBuckets // (zip [0 .. 3] $ cycle [1, 0])

-- QC42.3
doubleBiB :: UArray Int Int
doubleBiB = accum (*) updatedBiB $ zip [0 .. 3] $ cycle [2]

