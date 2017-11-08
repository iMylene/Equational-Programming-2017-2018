module Practicum1 where

{-
Name:           Mylène Martodihardjo
VU-net id:      mmo440
Student number: 2509676
Discussed with: Saskia Kreuzen
Remarks:        
Sources:        http://zvon.org/other/haskell/Outputprelude/foldr_f.html
				http://learnyouahaskell.com/starting-out#an-intro-to-lists
-}

-- Below you will find templates for the exercises. For each exercise,
-- replace 'undefined' by your definition and supply at least two different
-- meaningful tests to show that your code produces sane results. The
-- tests may be given in comments (see exercise 1).

-- Exercise 1
maxi :: Integer -> Integer -> Integer
maxi a b = if a >= b then a else b

-- maxi 2 3 == 3
-- maxi 3 2 == 2

-- Exercise 2
fourAscending :: Integer -> Integer -> Integer -> Integer -> Bool
fourAscending a b c d = if (a<b) && (b<c) && (c<d) then True else False 

-- fourAscending 1 2 3 4 == True
-- fourAscending 1 2 3 3 == False

-- Exercise 3
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = if (a==b) && (b==c) && (c==d) then True else False

-- fourEqual 1 1 3 4 == False
-- fourEqual 1 1 1 1 == True

-- Exercise 4
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent a b c d = if (a==b) || (a==c) || (a==d) || (b==c) || (b==d) || (c==d) then False else True

-- fourDifferent 1 2 3 4 == True
-- fourDifferent 1 1 3 4 == False

-- Exercise 5
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent a b c = ( ( a /= b) && (b /= c) )
{-
threeDifferent 1 0 1
-}

-- Exercise 6
factorial :: Integer -> Integer
factorial n = 
     if n < 2
        then 1
        else n * factorial (n-1)

-- factorial 0 == 1
-- factorial 5 == 120

-- Exercise 7
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib a = fib (a-1) + fib (a-2)

-- fib 2 == 1
-- fib 4 == 3

-- Exercise 8
-- it is possible to define auxiliary functions
strangeSummation :: Integer -> Integer
strangeSummation n = sum [n..n+7]

-- strangeSummation 0 == 27
-- strangeSummation 2 == 44

-- Exercise 9
lengthList :: [Integer] -> Integer
lengthList []  = 0
lengthList (h:t) = 1 + lengthList t

lengthListAlternative :: [Integer] -> Integer
lengthListAlternative l =
  case l of
    [] -> 0
    (h:t) -> 1 + (lengthListAlternative t)

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (h:t) = h + (sumList t)

-- sumList [1,2,3] == 6
-- sumList [3,3,3] == 9

-- Exercise 10
doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (h:t) = 2*h:(doubleList t)

-- doubleList [2,4,6] == [4,8,12]
-- doubleList [3] == [6]

-- Exercise 11
myappend :: [a] -> [a] -> [a]
myappend listA [] = listA
myappend listA (h:t) = myappend (reverse(h:reverse listA)) t

-- myappend [1,2,3] [4,5,6]
-- myappend [4,5,6] [20]

-- Exercise 12
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:t) = myappend (myreverse t) [h]

-- myreverse [9,8,7,6] == [6,7,8,9]
-- myreverse [3,4,5] == [5,4,3]

-- Exercise 13
mymember :: (Eq a) => a -> [a] -> Bool
mymember a [] = False
mymember a (h:t) = if a==h then True else mymember a t

-- mymember 5 [5,6,7] == True
-- mymember 0 [5,6,7] == False

-- Exercise 14
mysquaresum :: [Integer] -> Integer
mysquaresum [] = 0
mysquaresum (h:t) = h^2 + mysquaresum (t)

-- mysquaresum [2,3] == 13
-- mysquaresum [5,6,7] == 110

-- Exercise 15
range :: Integer -> Integer -> [Integer]
range a b =
     if a<=b
        then a:(range (a+1) b)
        else []

-- range 2 4 == [2,3,4]
-- range 10 10 == [10]

-- Exercise 16
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (h:t) = h ++ myconcat (t)

-- myconcat ["Hello","World","!"] == "HelloWorld!"
-- myconcat [[3],[1],[4]] == [3,1,4]

-- Exercise 17
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (h:t) =
     if a <= h
        then myconcat [[a], (h:t)]
        else myconcat [[h], insert a t] 
        
-- insert 4 [1,2,3,5]
-- insert 7 [1,2,4,10]
        
insertionsort :: Ord a => [a] -> [a]
insertionsort [] = []
insertionsort (h:t) = insert h (insertionsort t) 

-- insertionsort [1,3,2]
-- insertionsort [10, 8, 9 ,5]

-- Exercise 18
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:t) = quicksort (filter (\x -> x <= p) t) ++ (p:(quicksort (filter (\x -> x > p) t)))

-- quicksort [5,2,4,9,1,7] == [1,2,4,5,7,9]
-- quicksort [7,6,5] == [5,6,7]

-- Exercise 19
evensB :: [Integer] -> [Integer]
evensB a = [b | b <- a, (mod b 2) == 0]

-- evensB [10,2,3,5,7,1] == [10,2]
-- evensB [0,1] == [0]

-- Exercise 20
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (h:t) = f h:(mymap f t)

-- mymap (\x -> x+10) [1,2,3] == [11,12,13]
-- mymap (\x -> not x) [True,False,not True] == [False,True,True]

-- Exercise 21
twice :: (a -> a) -> a -> a
twice f a = f (f a)

-- twice (\x -> x+10) 3 == 23
-- twice (\x -> not x) True == True

-- Exercise 22
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g a = f(g a)

-- compose (\x->x*5) (\x->x+4) 7 == 55
-- compose (\x->x+4) (\x->x*5) 7 == 39

-- Exercise 23
mylast :: [a] -> a
mylast listA = head(reverse listA)

-- mylast [7,8,9] == 9
-- mylast [1..10] == 10

-- Exercise 24
mylastb :: [a] -> a
mylastb listA = head (drop ((length listA)-1) listA)

-- mylastb [7,8,9] == 9
-- mylastb [1..10] == 10

-- Exercise 25
myinit, myinitb :: [a] -> [a]
myinit listA = reverse (tail (reverse listA))
myinitb listA = take ((length listA)-1) listA

-- myinit [7,8,9] == [7,8]
-- myinitb [1..10] == [1,2,3,4,5,6,7,8,9]

-- Exercise 26
mysecondconcat :: [[a]] -> [a]
mysecondconcat listA = foldr (++) [] listA

mysecondreverse :: [a] -> [a]
mysecondreverse listA = foldr (\x y -> y ++ [x]) [] listA

-- mysecondconcat [[1,2,3],[5,6]] == [1,2,3,5,6]
-- mysecondreverse [1,2,3,4,5] == [5,4,3,2,1]

-- Exercise 27
prefix :: [a] -> [[a]]
prefix [] = [[]]
prefix listA = (prefix (init listA)) ++ [listA]

-- prefix [1,2,3] == [[],[1],[1,2],[1,2,3]]
-- prefix [True,True,False] == [[],[True],[True,True],[True,True,False]]