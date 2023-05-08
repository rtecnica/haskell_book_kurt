-- Q27.1
data Box a = Box a deriving Show

instance Functor Box where
   fmap :: (a -> b) -> Box a -> Box b 
   fmap func (Box item) = Box (func item)


copyItems :: a -> Int -> [a]
copyItems item n = map (\_ -> item) [1..n] 

morePresents :: Box a -> Int -> Box [a]
morePresents item n = (flip copyItems) n <$> item

-- Q27.2    
myBox :: Box Int
myBox = Box 1

wrap :: a -> Box a
wrap a = Box a

unwrap :: Box a -> a
unwrap (Box a) = a


