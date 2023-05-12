import Data.Map qualified as Map

type UserName = String

type GamerId = Int

type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB =
  Map.fromList
    [ (1, "nYarlathoTep"),
      (2, "KINGinYELLOW"),
      (3, "dagon1997"),
      (4, "rcarter1919"),
      (5, "xCTHULHUx"),
      (6, "yogSOThoth")
    ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB =
  Map.fromList
    [ ("nYarlathoTep", 2000),
      ("KINGinYELLOW", 15000),
      ("dagon1997", 300),
      ("rcarter1919", 12),
      ("xCTHULHUx", 50000),
      ("yogSOThoth", 150000)
    ]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB =
  Map.fromList
    [ (1001, 1),
      (1002, 2),
      (1003, 3),
      (1004, 4),
      (1005, 5),
      (1006, 6)
    ]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

-- class Applicative m => Monad (m :: * -> *) where
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>) :: m a -> m b -> m b
-- return :: a -> m a
-- fail :: String -> m a

-- QC30.4
ioNum :: Num a => a -> IO a
ioNum a = return ((+ 2) a)

-- Q30.1 To prove that Monad is strictly more powerful than Functor, write a universal version
-- of <$>, as in the preceding lesson’s exercise, called allFmapM, that defines <$> for all
-- members of the Monad type class. Because it works for all instances of Monad, the only functions
-- you can use are the methods required by the Monad type class (and lambda functions).
-- To get you started, here’s your type signature:

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f m = m >>= (\a -> return (f a))

-- Q30.2 To prove that Monad is strictly more powerful than Applicative, write a universal
-- version of <*>, called allApp, that defines <*> for all members of the Monad type class.
-- Because it works for all instances of Monad, the only functions you can use are the methods
-- required by the Monad type class (and lambda functions). To get you started, here’s
-- your type signature:
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp func val = func >>= (\f -> val >>= (\x -> return (f x)))

-- m (a -> b) -> ((a -> b) -> m b) -> mb

-- This question is much trickier than the last one. Two hints:
--  Try to think exclusively in terms of the type signatures.
--  Use <$> if you want and replace it with your answer to Q29.1

-- Q30.3 Implement a bind function which is the same as (>>=) for Maybe:
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just m) f = f m
