minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
    putStrLn "Enter three numbers"
    minInt <- minOfInts
    putStrLn (show minInt ++ " is the smallest")

-- QC28.4
maybeTen :: Maybe Int
maybeTen = Just 10
maybeThree :: Maybe Int
maybeThree = Just 3
maybeSix :: Maybe Int
maybeSix = Just 6

minOfMaybeInts :: Maybe Int
minOfMaybeInts = minOfThree <$> maybeTen <*> maybeThree <*> maybeSix
