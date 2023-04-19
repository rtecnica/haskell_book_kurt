import Data.Char

type Bits = [Bool]

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool,Bool) -> Bool
xorPair (v1,v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
    then False : intToBits' nextVal
    else True : intToBits' nextVal
        where remainder = n `mod` 2
              nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (intToBits' n)
          missingBits = maxBits - (length reversedBits)
          leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
    where size = length bits
          indices = [size-1,size-2 .. 0]
          trueLocations = filter (\x -> fst x == True)
                          (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair ->
                            (fst pair) `xor` (snd pair))
                        (zip padBits plaintextBits)
    where padBits = map charToBits pad
          plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
    where bitList = applyOTP' pad plaintext

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
    where halfN = n `div` 2
          offset = if even n
          then fromEnum c + halfN
            else 1 + fromEnum c + halfN
          rotation = offset `mod` n

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum c + halfAlphabet
          rotation = offset `mod` alphabetSize

rotEncoder :: String -> String
rotEncoder text = map rotChar text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotCharDecoder = rotNdecoder alphaSize

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data OneTimePad = OTP String

instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a*seed + b) `mod` maxNumber

myPrng :: Int -> Int
myPrng = prng (div maxChar 1234) 4321 maxChar 
    where maxChar = ord (maxBound :: Char)

prngStream :: Int -> Int -> [Int]
prngStream length seed = 
    if length == 0
        then [myPrng seed]
    else prngStream (length - 1) (myPrng seed) ++ [myPrng  seed]

prngStreamText :: Int -> Int -> String
prngStreamText length seed = map chr (prngStream length seed)

data StreamCypher = SC Int

instance Cipher StreamCypher where
    encode (SC seed) text = applyOTP bitstream text
        where bitstream = prngStreamText (length text) seed

    decode (SC seed) text = encode (SC seed) text