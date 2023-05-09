import Data.Map qualified as Map
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO
import GHC.IO.Device qualified as T
import Text.Read qualified as TR

data RobotPart = RobotPart
  { name :: String,
    description :: String,
    cost :: Double,
    count :: Int
  }
  deriving (Show)

leftArm :: RobotPart
leftArm =
  RobotPart
    { name = "left arm",
      description = "left arm for face punching!",
      cost = 1000.00,
      count = 3
    }

rightArm :: RobotPart
rightArm =
  RobotPart
    { name = "right arm",
      description = "right arm for kind hand gestures",
      cost = 1025.00,
      count = 5
    }

robotHead :: RobotPart
robotHead =
  RobotPart
    { name = "robot head",
      description = "this head looks mad",
      cost = 5092.25,
      count = 2
    }

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

-- Q28.3 
-- Make a command-line application that has a database of various RobotParts (at least five),
-- and then lets the user enter in two-part IDs and returns the one with the lowest cost.
-- Handle the case of the user entering an ID that’s not in the parts database.
-- TODO: This shit