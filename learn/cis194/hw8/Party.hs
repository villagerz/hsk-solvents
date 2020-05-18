{-#       OPTIONS_GHC -fno-warn-orphans #-}
module Party where
import Employee
import Data.Tree
import Data.List (sortBy)


glCons :: Employee -> GuestList -> GuestList
glCons e gl = GL (e:(glList gl)) (glFun gl + (empFun e))

glFun :: GuestList -> Fun
glFun (GL _ fun) = fun

glList :: GuestList -> [Employee]
glList (GL xs _) = xs

instance Semigroup GuestList where
  (<>) = mappend
  
instance Monoid GuestList where
  mempty = GL [] 0
  mappend a b = foldr (glCons) b (glList a)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b

treeFold f  (Node x [])  = f x []  
treeFold f  (Node v x)  = f v sublist
                          where sublist = [ treeFold f subtr | subtr <- x]

-- (with, without)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = (glCons e (GL [] 0), GL [] 0)
nextLevel boss gls = (glCons boss maxwithout, maxwithsub)
  where (maxwithsub, maxwithout) = foldr1 (<>) gls
--        globalmax (a,b) (c,d) = (( max a c ), (max b d))
  

maxFun' :: Tree Employee -> GuestList
maxFun' tr = uncurry moreFun rootlists
  where rootlists = treeFold (nextLevel)  tr

sortedNames :: GuestList -> String
sortedNames gl = unlines $ map (empName) (sortBy firstNameCmp (glList gl))
  where firstNameCmp e1 e2 = compare (empName e1) (empName e2)

main :: IO ()
main = do
  putStrLn "testing"
  treeStr <- readFile "company.txt"
  let gl = maxFun' (read treeStr)
  putStrLn ("Total Fun:" ++ show (glFun gl))
  putStrLn (sortedNames gl)
  putStrLn "end"
