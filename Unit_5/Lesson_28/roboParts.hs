{-# LANGUAGE OverloadedStrings #-}

import Data.Map qualified as Map
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO
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

robotLegs :: RobotPart
robotLegs =
  RobotPart
    { name = "robot legs",
      description = "these legs run fast",
      cost = 7098.33,
      count = 7
    }

robotTorso :: RobotPart
robotTorso =
  RobotPart
    { name = "robot torso",
      description = "this torso is bulletproof",
      cost = 15000.3,
      count = 3
    }

robotGun :: RobotPart
robotGun =
  RobotPart
    { name = "robot torso",
      description = "this torso is bulletproof",
      cost = 15000.3,
      count = 3
    }

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    vals = [leftArm, rightArm, robotHead, robotTorso, robotLegs, robotGun]
    keys = [1 .. length vals]
    keyVals = zip keys vals

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

partVal :: Int -> Maybe RobotPart
partVal = flip Map.lookup partsDB

-- Q28.3
-- Make a command-line application that has a database of various RobotParts (at least five),
-- and then lets the user enter in two-part IDs and returns the one with the lowest cost.
-- Handle the case of the user entering an ID that’s not in the parts database.

main :: IO ()
main = do
  print "Enter two part numbers:"
  input <- TIO.getContents
  let lines = T.lines input
  mapM_ (myIOprint . maybeMinCost . parse) lines

maybeMinCost :: Maybe [Int] -> Maybe String
maybeMinCost Nothing = Nothing
maybeMinCost (Just list) = minCost <$> partVal (head list) <*> partVal (list !! 1)

minCost :: RobotPart -> RobotPart -> String
minCost firstItem secondItem = case compare firstCost secondCost of
  GT -> show firstItem
  LT -> show secondItem
  EQ -> show firstItem ++ "\n" ++ show secondItem
  where
    firstCost = cost firstItem
    secondCost = cost secondItem

parse :: T.Text -> Maybe [Int]
parse line =
  if length split /= 2
    then Nothing
    else Just (map (read . T.unpack) split)
  where
    split = map T.strip (T.splitOn " " line)

myIOprint :: Maybe String -> IO ()
myIOprint Nothing = print "Nonexistant Parts, try again..."
myIOprint (Just string) = do
  if string == ""
    then return ()
    else putStrLn string
