toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1 = []
  | n < 10 = [n]
  | otherwise = (mod n 10) : toDigitsRev ( div n 10 )

toDigits :: Integer -> [Integer]
toDigits n = reverse ( toDigitsRev n )

-- requirement is to double every other number from the RIGHT
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (applyToOther (2*) (reverse x))

applyToOther :: (Integer -> Integer) -> [Integer] -> [Integer]
applyToOther _ [] = []
applyToOther _ [x] = [ x ]
applyToOther f (x:y:ys) = x:f y:applyToOther f ys


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs)
  | x < 10 = x + sumDigits xs
  | otherwise = sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = ((mod . sumDigits . doubleEveryOther $ toDigits n) $ 10) == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b tmp
  | n <= 0 = []
  | n == 1 = [(a,b)]
  | otherwise = hanoi (n - 1) a tmp b ++ [(a,b)] ++ hanoi (n-1) tmp b a
  
