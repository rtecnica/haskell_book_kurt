-- QC11.2
printDouble :: Int -> String
printDouble int =
  show (int * 2)

-- Q11.1
-- filt :: (a -> a) -> [a] -> [a]

-- Q11.2
myFoldl :: (t -> a -> t) -> t -> [a] -> t
myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInit xs
  where
    newInit = f init x
