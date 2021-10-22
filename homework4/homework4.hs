module Homework4 where

-- exercise 1 ------------------------------------------------------------------
fun1 :: [Integer] -> Integer
fun1 []       = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' xs
  | filter even xs == [] = 1  -- no even numbers
  | elem 2 xs            = 0  -- contains a 2 (makes the overall product 0)
  | otherwise            = product $ reducedevens
  where
    evenslargerthan2 = filter even xs
    reducedevens     = map (subtract 2) evenslargerthan2

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3*n + 1)

fun2' :: Integer -> Integer
fun2' n = sum $ takeWhile (>= 1) $ filter even $ iterate collatz n

collatz :: Integer -> Integer
collatz 1 = 0
collatz n
  | even n    = n `div` 2
  | otherwise = 3*n + 1

-- exercise 2 ------------------------------------------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- write a function which generates a balanced binary tree from a
-- list of values using foldr
-- (note that the tree must be balanced, but not sorted.)

foldTree :: (Ord a) => [a] -> Tree a
foldTree as = foldr foldHelper Leaf as

foldHelper :: (Ord a) => a -> Tree a -> Tree a
foldHelper newvalue tree = updateHeight $ addToTree newvalue tree

updateHeight :: (Ord a) => Tree a -> Tree a
updateHeight Leaf                     = Leaf
updateHeight tree@(Node h lc val rc) = (Node nh (updateHeight lc) val (updateHeight rc))
  where
    nh = height tree

-- find a node or subtree to insert into,
-- does not update height of tree
addToTree :: (Ord a) => a -> Tree a -> Tree a
addToTree val Leaf = (Node 0 Leaf val Leaf)
addToTree val tree@(Node nh lc nv rc)
  | lc == Leaf && rc == Leaf = (Node nh (addToTree val lc) nv rc)
  | lc == Leaf               = (Node nh (addToTree val lc) nv rc)
  | rc == Leaf               = (Node nh lc nv (addToTree val rc))
  | leftnodes < rightnodes   = (Node nh (addToTree val lc) nv rc)
  | leftnodes > rightnodes   = (Node nh lc nv (addToTree val rc))
  | otherwise                = (Node nh (addToTree val lc) nv rc) -- default
  where
    leftnodes       = numNodes lc
    rightnodes      = numNodes rc

-- given a tree, calculates the number of nodes under it
numNodes :: Num p => Tree a -> p
numNodes Leaf = 0
numNodes (Node _ lc _ rc) = 1 + (numNodes lc) + (numNodes rc)

height :: (Ord a) => Tree a -> Integer
height Leaf = 0
height (Node _ Leaf _ Leaf) = 0
height (Node _ lc   _ rc  ) = 1 + max (height lc) (height rc)


-- exercise 3 ------------------------------------------------------------------

-- (1)
-- implement a function which returns True iff there are an odd number of True
-- values contained in the input list, must be implemented with a fold

xor :: [Bool] -> Bool
xor vs = foldr xorHelper False vs
  where
    xorHelper :: Bool -> Bool -> Bool
    xorHelper v1 v2
      | v1 && v2  = not v1
      | v1        = v1
      | otherwise = v2

-- (2)
-- Implement map' as a fold s.t. map' behaves identically to standard map

map' :: (a -> b) -> [a] -> [b]
map' f vs = foldr (helper f) [] vs
  where
    helper :: (a -> b) -> a -> [b] -> [b]
    helper f a bs = (f a) : bs

-- (3)
-- Implement foldl using foldr

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)


-- exercise 4 ------------------------------------------------------------------

-- implement the Sieve of Sundaram using function composition
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = foldr inCommon [1.. (2*n +2)] $ map (multipleRemover [1..(2*n+2)]) [2..(2*n +1)]

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [ (x,y) | x <- xs, y <- ys]

multipleRemover :: [Integer] -> Integer -> [Integer]
multipleRemover us i = [ x | x <- us, mod x i > 0 || x == i]

inCommon :: (Eq a) => [a] -> [a] -> [a]
inCommon xs ys = [ x | x <- xs, y <- ys, x == y]



-- addToTree newvalue Leaf = (Node 0 Leaf newvalue Leaf)
-- addToTree newvalue tree
--   | lc == Leaf && rc == Leaf = (Node (h+1) (Node (h) Leaf newvalue Leaf) nv rc) -- node has no subtrees, add to left leaf by default
--   | lc == Leaf               = (Node (h+1) (Node (h) Leaf newvalue Leaf) nv rc) -- new value added to left leaf
--   | rc == Leaf               = (Node (h+1) rc nv (Node (h) Leaf newvalue Leaf)) -- new value added to right leaf
--   | bf == -1                 = (Node (h+1) (addToTree newvalue lc) nv rc)       -- value pushed to left subtree
--   | bf ==  1                 = (Node (h+1) lc nv (addToTree newvalue rc))       -- value pushed to right subtree
--   | otherwise                = (Node (h+1) lc nv (addToTree newvalue rc))       -- by default, push value to right subtree
--   where
--     nv  = value tree     -- value at current node
--     lc  = leftchild tree
--     rc  = rightchild tree
--     h   = height tree
--     bf  = (height lc) - (height rc)


