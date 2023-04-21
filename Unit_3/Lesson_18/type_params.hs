import Data.Map qualified as Map
import Data.Tuple qualified as Tuple

organs :: [Organ]
organs = [Heart, Brain, Kidney, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

data Triple a = Triple a a a deriving (Show)

data Box a = Box a deriving (Show)

-- Q18.1
tripleMap :: (a -> a) -> Triple a -> Triple a
tripleMap func (Triple a b c) = Triple (func a) (func b) (func c)

boxMap :: (a -> a) -> Box a -> Box a
boxMap f (Box a) = Box (f a)

-- Q18.2
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (map Tuple.swap organPairs)