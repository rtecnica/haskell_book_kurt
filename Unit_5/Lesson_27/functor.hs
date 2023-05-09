import Data.Map qualified as Map
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO
import GHC.IO.Device qualified as T
import Text.Read qualified as TR

-- QC27.1
reverseMaybe :: Maybe String -> Maybe String
-- reverseMaybe Nothing = Nothing
-- reverseMaybe (Just string) = Just (reverse string)
-- reverseMaybe = fmap reverse
reverseMaybe = (<$>) reverse

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

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part =
  mconcat
    [ "<h2>",
      partName,
      "</h2>",
      "<p><h3>desc</h3>",
      partDesc,
      "</p><p><h3>cost</h3>",
      partCost,
      "</p><p><h3>count</h3>",
      partCount,
      "</p>"
    ]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

-- Q27.3
main :: IO ()
main = do
  input <- TIO.getContents
  let lines = map (TR.readMaybe . T.unpack) (T.lines input)
  mapM_ (myIOprint . parse . func) lines

func :: Maybe Int -> Maybe Double
func Nothing = Nothing
func (Just i) = cost <$> Map.lookup i partsDB

parse :: Maybe Double -> String
parse (Just a) = show a
parse Nothing = ""

myIOprint :: String -> IO ()
myIOprint string = do
  if string == ""
    then return ()
    else print string
