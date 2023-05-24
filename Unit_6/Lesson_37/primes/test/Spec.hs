import Data.Maybe (fromJust, isJust, isNothing)
import Primes (isPrime, primes)
import Test.QuickCheck
  ( Args (maxSuccess),
    quickCheck,
    quickCheckWith,
    stdArgs,
  )

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs {maxSuccess = 1000} prop_primesArePrime
  quickCheckWith stdArgs {maxSuccess = 1000} prop_nonPrimesAreComposite
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime

prop_validPrimesOnly :: Int -> Bool
prop_validPrimesOnly val =
  if val <= 0 || val >= length primes
    then isNothing result
    else isJust result
  where
    result = isPrime val

prop_primesArePrime :: Int -> Bool
prop_primesArePrime val =
  if result == Just True
    then length divisors == 0
    else True
  where
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite :: Int -> Bool
prop_nonPrimesAreComposite val =
  if result == Just False
    then length divisors > 0
    else True
  where
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next : primes) =
  if n `mod` next == 0
    then next : unsafePrimeFactors (n `div` next) (next : primes)
    else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n
  | n < 2 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where
    primesLessThanN = filter (<= n) primes

prop_factorsMakeOriginal :: Int -> Bool
prop_factorsMakeOriginal val =
  if result == Nothing
    then True
    else product (fromJust result) == val
  where
    result = primeFactors val

prop_allFactorsPrime :: Int -> Bool
prop_allFactorsPrime val =
  if result == Nothing
    then True
    else all (== Just True) resultsPrime
  where
    result = primeFactors val
    resultsPrime = map isPrime (fromJust result)