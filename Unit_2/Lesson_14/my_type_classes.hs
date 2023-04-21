-- Q14.1
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum)

instance Eq SixSidedDie where
  (==) x y = fromEnum x == fromEnum y

instance Ord SixSidedDie where
  compare x y = compare (fromEnum x) (fromEnum y)

-- Q14.2
data FiveSidedDie = F1 | F2 | F3 | F4 | F5 deriving (Eq, Ord, Enum)

class (Ord a, Eq a, Enum a) => Die a where
  rollDie :: a -> String

instance Die FiveSidedDie where
  rollDie die =
    "No"
