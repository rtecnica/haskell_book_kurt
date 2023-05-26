{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import Data.Text as T
import GHC.Generics
import Control.Monad

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults

data Book = Book
  { title :: T.Text,
    author :: T.Text,
    year :: Int
  }
  deriving (Show, Generic)

instance FromJSON Book

instance ToJSON Book

myBook :: Book
myBook =
  Book
    { author = "Will Kurt",
      title = "Learn Haskell",
      year = 2017
    }

myBook2 :: Book
myBook2 =
  Book
    { author = "Emil Ciroan",
      title = "A Short History of Decay",
      year = 1949
    }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

myBook2JSON :: BC.ByteString
myBook2JSON = encode myBook2

rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromJSON :: Either String Book
bookFromJSON = eitherDecode rawJSON

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

data ErrorMessage = ErrorMessage
  { message :: T.Text,
    errorCode :: Int
  }
  deriving (Show)

instance FromJSON ErrorMessage where
  parseJSON (Object v) =
    ErrorMessage
      <$> v
      .: "message"
      <*> v
      .: "error"

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

data NOAAResult = NOAAResult
  { uid :: T.Text,
    mindate :: T.Text,
    maxdate :: T.Text,
    name :: T.Text,
    datacoverage :: Float,
    resultId :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult
      <$> v
      .: "uid"
      <*> v
      .: "mindate"
      <*> v
      .: "maxdate"
      <*> v
      .: "name"
      <*> v
      .: "datacoverage"
      <*> v
      .: "id"

data Resultset = Resultset
  { offset :: Int,
    count :: Int,
    limit :: Int
  }
  deriving (Show, Generic)

instance FromJSON Resultset

data Metadata = Metadata
  { resultset :: Resultset
  }
  deriving (Show, Generic)

instance FromJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata,
    results :: [NOAAResult]
  }
  deriving (Show, Generic)

instance FromJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just reesults) = do
  forM_ reesults (print . name)
--   print dataName

-- Q40.1 Make your NOAAResponse type an instance of ToJSON. This requires making all the
-- types used by this type instances of ToJSON as well.
instance ToJSON Resultset
instance ToJSON Metadata
instance ToJSON NOAAResult
instance ToJSON NOAAResponse


-- Q40.2 Make a Sum type called IntList and use DerivingGeneric to make it an instance of
-- ToJSON. Don’t use the existing List type, but rather write it from scratch. Here’s an example
-- of an IntList:

data IntList = Cons Int IntList | EmptyList deriving (Show, Generic)

intListExample :: IntList
intListExample = Cons 1 $
    Cons 2 EmptyList

instance ToJSON IntList

intListJSON = toJSON intListExample
