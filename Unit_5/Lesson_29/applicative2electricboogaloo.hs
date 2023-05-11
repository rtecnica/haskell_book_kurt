primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where
    twoThroughN = [2 .. n]
    composite = pure (*) <*> twoThroughN <*> twoThroughN
    isNotComposite = not . (`elem` composite)

-- Q29.1 To prove that Applicative is strictly more powerful than Functor, write a universal
-- version of fmap, called allFmap, that defines fmap for all members of the Applicative type
-- class. Because it works for all instances of Applicative, the only functions you can use are
-- the methods required by the Applicative type class. To get you started, here’s your type
-- signature:
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func item = pure func <*> item

-- Q29.2 Translate the following expression into one where the result is a Maybe Int. The
-- catch is that you may not add (or remove) anything to the code except pure and <*>. You
-- can’t use the Just constructor or any extra parentheses.

example :: Int
example = (*) ((+) 2 4) 6

-- Here’s the type signature for your answer:
exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> (pure 2) <*> (pure 4)) <*> pure 6

-- Q29.3 Take the following example and use nondeterministic computing with Lists to
-- determine how much beer you need to purchase to assure there will be enough:
--
--  You bought beer last night but don’t remember whether it was a 6-pack or a 12-
-- pack.
--  You and your roommate each had two beers last night.
--  You’re having either two or three friends coming over tonight, depending on
-- who can come.
--  For a long night of gaming, you expect the average person to drink three to four
-- beers.

beerPacks :: [Int]
beerPacks = [6, 12]

beersHad :: [Int]
beersHad = [4]

beerFriends :: [Int]
beerFriends = [4, 5]

beerAvg :: [Int]
beerAvg = [3, 4]

maxList :: [Int] -> Int
maxList = foldl max minBound

minList :: [Int] -> Int
minList = foldl min maxBound

minBeer :: Int
minBeer = maxList beerNeed - minList beerRemainder
  where
    beerNeed = pure (*) <*> beerFriends <*> beerAvg
    beerRemainder = pure (-) <*> beerPacks <*> beersHad
