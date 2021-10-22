module Golf where

import Data.List

-- (1) Hopscotch ---------------------------------------------------------------

-- this function makes the actual list of indicies to select out of
-- our string, given the length of string u and "skips" i
-- e.g. helper 6 3 returns indicies [2,5]
-- note there is a funny "off by one" thing happening with the mod
-- so "1 skip" correlates to "no skips" i.e. indicies [0..u-1], and "u skips"
-- correlates to index (u-1)
helper :: Int -> Int -> [Int]
helper u i
  | i >  u    = error "List index out of bounds: make sure first parameter is \
                      \ smaller than or equal to the second parameter."
  | i <= 0    = []
  | i == 1    = [0..u-1]
  | otherwise = [ x | x <- [0..u-1], x `mod` i == (i-1)]

-- this function takes a list of as and an integer i, and selects out every ith
-- element from the list
helper2 :: [a] -> Int -> [a]
helper2 as i = map (as !!) $ helper u i
  where u = length as

-- this function takes a list of partially applied functions
-- that need an a and return a list of bs,
-- a list of as, and returns a list of bs
maplistfunctions :: [(a -> [b])] -> [a] -> [[b]]
maplistfunctions [] _ = []
maplistfunctions _ [] = []
maplistfunctions (f:fs) (a:as) = f a : maplistfunctions fs as

skips :: [a] -> [[a]]
skips as = maplistfunctions partialed [1..l]
  where
    l         = length as
    entries   = replicate l as
    partialed = map helper2 entries

-- (2) Local maxima ------------------------------------------------------------

recursiveLocalMaxima :: [Integer] -> [Integer]
recursiveLocalMaxima (n1:[])    = []
recursiveLocalMaxima (n1:n2:[]) = []
recursiveLocalMaxima (n1:n2:n3:[])
  | n1 < n2 && n2 > n3 = [n2]
  | otherwise          = []
recursiveLocalMaxima (n1:n2:n3:ns)
  | n1 < n2 && n2 > n3 = n2 : recursiveLocalMaxima (n3:ns)
  | otherwise          = recursiveLocalMaxima (n2:n3:ns)

localMaxima :: [Integer] -> [Integer]
localMaxima ns = sort $ recursiveLocalMaxima ns

-- (3) Histogram ---------------------------------------------------------------

testlist = [1,4,5,4,6,6,3,4,2,4,9]

histogram :: [Integer] -> String
histogram digits = (stars digits) ++ "==========\n0123456789\n"
  where
    stars :: [Integer] -> String
    stars digits = unlines $ reverse $ transpose $ histogramHelper $ elementCount digits

getNumOcc :: [Integer] -> Integer -> Int
getNumOcc as a = length $ filter (==a) as

elementCount :: [Integer]  -> [Int]
elementCount as = map (getNumOcc as) [0..9]

histogramHelper :: [Int] -> [String]
histogramHelper items = map (produceString longest) items
  where
    longest = maximum items

produceString :: Int -> Int -> String
produceString maxlength reps
  | reps > maxlength = error "Repetitions larger than maximum length."
  | otherwise        = concat $ replicate reps "*" ++ replicate (maxlength - reps) " "
