module Practicum2A where

{-
Name:           Mylène Martodihardjo
VU-net id:      mmo440
Student number: 2509676
Discussed with: Saskia Kreuzen
Remarks:        
Sources:        https://rosettacode.org/wiki/Towers_of_Hanoi#Haskell
		http://zvon.org/other/haskell/Outputprelude/map_f.html
		https://techoverflow.net/2014/01/03/haskell-invert-filter-predicate/
		http://zvon.org/other/haskell/Outputprelude/zipWith_f.html
		https://wiki.haskell.org/The_Fibonacci_sequence#Canonical_zipWith_implementation
		https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell
-}

----------------------------
-- Exercise Tower of Hanoi
----------------------------

type Rod = String
type Move = (Integer, Rod, Rod)
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n-1) a c b ++ [(n,a,b)] ++ hanoi (n-1) c b a

-- Each n represents the size of the disc, so the biggest discs has the highest value.
-- We start in rod1 and with the use of rod3, the tower ends in rod2

-- -------------------------
-- Exercises Infinite Lists
-- -------------------------

-- Exercise 1
naturals :: [Integer]
naturals = 1 : map (+1) naturals

-- Exercise 2
zeroesandones :: [Integer]
zeroesandones = 0 : 1 : zeroesandones

-- Exercise 3
threefolds :: [Integer]
threefolds = 0 : map (*3) naturals

-- Exercise 4
removeif :: (a -> Bool) -> [a] -> [a]
removeif func = filter (not.func)

nothreefolds :: [Integer]
nothreefolds = removeif (\x -> mod x 3 == 0) naturals

-- Exercise 5
allnfolds :: Integer -> [Integer]
allnfolds n = 0*n : map (*n) naturals

-- Exercise 6
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds 0 = naturals
allnaturalsexceptnfolds 1 = []
allnaturalsexceptnfolds n = removeif (\x -> mod x n == 0) naturals

-- Exercise 7
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds 0 listA = filter (/=0) listA
allelementsexceptnfolds n listA = removeif (\x -> mod x n == 0) listA

-- Exercise 8
eratosthenes :: [Integer]
eratosthenes = sieve [2..]
            where sieve (p:listN) = p : sieve [n | n <- listN, mod n p /= 0]

-- Exercise 9
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- -----------------------
-- Exercise Church Numerals
-- -----------------------

type ChurchNumeral a = (a -> a) -> a -> a
type FuncOneArg  a = ChurchNumeral a -> ChurchNumeral a
type FuncTwoArgs a = ChurchNumeral a -> ChurchNumeral a -> ChurchNumeral a

-- Exercise 1
churchnumeral :: (Eq a, Num a) => a -> ChurchNumeral a
churchnumeral n =
  if   n == 0
  then \s z -> z
  else \s z -> churchnumeral (n - 1) s (s z)

backtointeger :: (Num a) => ChurchNumeral a -> a
backtointeger cn = cn (+1) 0

{-
backtointeger (churchnumeral 5) == 5
backtointeger (churchnumeral 100) == 100

Bear in mind that churchnumeral is a function, which Haskell is not able to print.
This is the main reason we apply backtointeger after using churchnumeral.
-}

-- Exercise 2
churchequality :: (Eq a, Num a) => ChurchNumeral a -> ChurchNumeral a -> Bool
churchequality m n = backtointeger (m) == backtointeger (n)

-- Exercise 3
successor :: (Num a) => FuncOneArg a
successor n = (\x s z -> s (x s z)) n

-- Exercise 4
successorb :: (Num a) => FuncOneArg a
successorb n = (\x s z -> x s (s z)) n

-- Exercise 5
apply1 :: (Eq a, Num a) => FuncOneArg a -> a -> a
apply1 f n = backtointeger (f (churchnumeral n))

-- Exercise 6
addition :: (Num a) => FuncTwoArgs a
addition m n = (\x y s z -> x s (y s z)) m n

multiplication :: (Num a) => FuncTwoArgs a
multiplication m n = (\x y s -> x (y s)) m n

exponentiation :: (Num a) => FuncTwoArgs a
exponentiation = undefined

-- Exercise 7
-- given
-- apply2  that can be used for addition, multiplication, exponentiation
apply2 f n m = backtointeger (f (churchnumeral n) (churchnumeral m) ) 

-- ---------------------
-- Exercises Binary Trees
-- ---------------------

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

single :: a -> BinaryTree a
single x = Node (Leaf) x (Leaf)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes Leaf = 0
numberofnodes (Node leafone x leaftwo) = 1 + (numberofnodes leafone) + (numberofnodes leaftwo)

-- Exercise 2
height :: BinaryTree a -> Integer
height Leaf = 0
height (Node leafone x leaftwo) = 1 + (max (height leafone) (height leaftwo))

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes Leaf = 0
sumnodes (Node leafone x leaftwo) = 1 + (sumnodes leafone) + (sumnodes leaftwo)

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror Leaf = Leaf
mirror (Node leafone x leaftwo) = Node (mirror leaftwo) x (mirror leafone)

-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten Leaf = []
flatten (Node leafone x leaftwo) = (flatten leafone) ++ [x] ++ flatten (leaftwo)

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap f Leaf = Leaf
treemap f (Node leafone x leaftwo) = Node (treemap f leafone) (f x) (treemap f leaftwo)

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan a Leaf = True
smallerthan a (Node leafone x leaftwo) =
  if x >= a
    then False
  else (smallerthan a leafone) && (smallerthan a leaftwo)

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan a Leaf = True
largerthan a (Node leafone x leaftwo) =
  if x <= a
    then False
  else (largerthan a leafone) && (largerthan a leaftwo)

-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree Leaf = True
isbinarysearchtree (Node leafone x leaftwo) =
  (smallerthan x leafone) && (largerthan x leaftwo) &&
  (isbinarysearchtree leafone) && (isbinarysearchtree leaftwo)

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement a Leaf = False
iselement a (Node leafone x leaftwo) = 
  if a == x
    then True
  else (iselement a leafone) || (iselement a leaftwo)

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert a Leaf = (Node Leaf a Leaf)
insert a (Node leafone x leaftwo) = 
  if (iselement a (Node leafone x leaftwo))
    then (Node leafone x leaftwo)
  else if a < x
    then (Node (insert a leafone) x leaftwo)
  else (Node leafone x (insert a leaftwo))

-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree [] = Leaf
createbinarysearchtree (h:t) = (insert h (createbinarysearchtree t))

-- Exercise 6
delete :: Eq a => a -> [a] -> [a]
delete deleteThis xs = [x | x <- xs, x /= deleteThis]

remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove a Leaf = Leaf
remove a (Node leafone x leaftwo) = 
  if (iselement a (Node leafone x leaftwo))
    then createbinarysearchtree(delete a (flatten (Node leafone x leaftwo)))
  else (Node leafone x leaftwo)
