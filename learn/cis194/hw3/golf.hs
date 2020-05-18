-- hopscotch
-- list of lists, every nth element
-- contains every nth element from input list
-- [a b c d] = [[a b c d], [b d] , [c], [d]]

skips :: [a] -> [[a]]
skips l = [getnth' n l | n <- [1 .. length l]] 

getnth' :: Int -> [a] -> [a]
getnth' n l = [a | (a,i) <- zip l [1 ..], mod i n == 0]

-- local maxima
-- number that is greater than number to its left
-- and to its right

localMaxima :: [Integer] -> [Integer]
localMaxima l@(a:b:c:xs)
  | (b > a) && ( b > c) = b : localMaxima (tail l)
  | otherwise = localMaxima (tail l)
localMaxima (_:_) = []
localMaxima [] = []

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3*n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate series
  where series n = if even n then n `div` 2 else 3*n + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = undefined


addNode :: a -> Tree a -> Tree a
addNode nd Leaf = Node 0 Leaf nd Leaf
addNode nd t@(Node i Leaf n Leaf)  = Node 1 (addNode nd Leaf)  n Leaf
addNode nd t@(Node i l n Leaf)  = Node 1 l n (addNode nd Leaf )
addNode nd t@(Node i l val r) 
  | ht l > ht r = Node (newHt (a' r) l) l val (a' r)
  | otherwise = Node (newHt (a' l) r) (a' l) val r
  where ht (Node i _ _ _) = i
        ht _ = 0
        a' n = addNode nd n
        newHt n m = max (ht n) (ht m) + 1

xor' :: [Bool] -> Bool
xor' = foldl (\b a -> not b && a) False . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f a : b) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)
