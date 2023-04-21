{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char

data Creator = AuthorCreator Author | ArtistCreator Artist

data Author = Author Name

data Artist = Person Name | Band String

data Book = Book
  { author :: Creator,
    isbn :: String,
    bookTitle :: String,
    bookYear :: Int,
    bookPrice :: Double
  }

data VinylRecord = VinylRecord
  { artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
  }

data CollectibleToy = CollectibleToy
  { name :: String,
    descrption :: String,
    toyPrice :: Double
  }

-- Q16.1
data Pamphlet = Pamphlet
  { title :: String,
    description :: String,
    contact :: String
  }

data StoreItem = BookItem Book | RecordItem VinylRecord | PamphletItem Pamphlet | ToyItem CollectibleToy

-- Q16.2
data Circle = Circle
  { radius :: Double
  }

data Square = Square
  { side :: Double
  }

data Rectangle = Rectangle
  { sidea :: Double,
    sideb :: Double
  }

data Shape = CircleShape Circle | SquareShape Square | RectangleShape Rectangle

perimiter :: Shape -> Double
perimiter (CircleShape circle) = radius circle * pi
perimiter (SquareShape square) = 4 * side square
perimiter (RectangleShape rectangle) = 2 * sidea rectangle + 2 * sideb rectangle

area :: Shape -> Double
area (CircleShape circle) = pi * (radius circle) ^ 2
area (SquareShape square) = side square ^ 2
area (RectangleShape rectangle) = sidea rectangle * sideb rectangle