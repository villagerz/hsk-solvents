{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fibs1 :: [Integer]
fibs1 = [ fib f | f <- [0 ..]]

fibs2' :: [Integer]
fibs2' = 0 : 1 : zipWith (+) fibs2' (tail fibs2')

fibs2'' :: [Integer]
fibs2'' = map fst $ iterate (\(a, b) -> (b, a + b)) (0, 1)

data Stream a = a :~: (Stream a)

  
streamToList :: Stream a -> [a]
streamToList (s :~: ss) = s : streamToList ss

instance Show a => Show (Stream a) where
  show s = (show.(take 50). streamToList $ s) ++ " ..."

streamRepeat :: a -> Stream a
streamRepeat x = x :~: streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (s:~:ss) = (f s) :~: streamMap f ss

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = let s' = f seed in seed :~: streamFromSeed f s'


nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Define the streamruler :: Stream Integer
-- which corresponds to the ruler
-- function
-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
--  where
-- the nth element in the stream (assuming the first element corresponds
-- ton=1) is the largest power of 2 which evenly divides n.
-- if you remove the 0's (meaning you will interleave them later)
-- you get 1,2,1,3,1,2,1,4,..
-- now you can remove the 1's and interleave them later
-- you get 2,3,2,4,2,3,2,5,2,3,2,4,2...
-- that is pattern.
ruler :: Stream Integer
ruler = mfold nats
  where mfold (s:~:ss) = interleave (streamRepeat s) (mfold ss)

interleave :: Stream a -> Stream a -> Stream a
interleave (x:~:aa) bb = x  :~: interleave bb aa

x :: Stream Integer
x = 0 :~: (1:~: streamRepeat 0)


instance Num (Stream Integer) where
  fromInteger a = a :~: streamRepeat 0
  negate = streamMap negate
  (+) (s1:~:ss1) (s2:~:ss2) = (s1 + s2) :~: (ss1 + ss2)
  (*) a@(a1:~:at) b@(b1:~:bt) = (a1 * b1) :~: coeffStream
    where coeffStream = (streamMap (*a1) bt) + (at*b)
  abs = undefined
  signum = undefined
  
instance Fractional (Stream Integer) where
  (/) a@(a1:~:at) b@(b1:~:bt) = coeffStream
    where coeffStream = (div a1 b1) :~: streamMap ( `div` b1) (at - coeffStream * bt)

-- F(x) = (x/(1-x-x^2))
fibs3 :: Stream Integer
fibs3 = (x) / (1 - x - x^2)

-- The MATRIX approach: --
type  MRow = (Integer, Integer) 
data Matrix = M MRow MRow deriving (Show)


instance Num Matrix where
  (*) (M (a,b) (c,d)) (M (w,x)(y,z)) = M ((rc a b w y), (rc a b x z)) ((rc c d w y), (rc c d x z))
    where rc r1 r2 c1 c2 = r1 * c1 + r2 * c2

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = diagOf $ (M (1,1) (1,0)) ^ n
  where diagOf (M (_,b) _) = b
