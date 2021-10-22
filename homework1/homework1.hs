-- homework1.hs

module Homework where

import Data.Char

-- for problem 2
type Peg = String
type Move = (Peg, Peg)

-- problem 1 below
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | n <= 9    = [n]
  | otherwise = lastdigit : toDigitsRev initialDigits
    where
      lastdigit     = n `mod` 10
      initialDigits = (n - lastdigit) `div` 10

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []   = []         -- Empty list case
doubleEveryOther [x]  = [x]       -- Single element case
doubleEveryOther xs   = reverse result
  where
    result = doubleEveryOther' $ reverse xs
    doubleEveryOther' :: [Integer] -> [Integer]
    doubleEveryOther' [] = []
    doubleEveryOther' [x] = [x]
    doubleEveryOther' (x:y:zs) =
      x : y*2 : doubleEveryOther' zs

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 result
  where
    result = concat $ map toDigits xs

validate :: Integer -> Bool
validate n
  | result `mod` 10 == 0 = True
  | otherwise            = False
  where
    result = sumDigits $ doubleEveryOther $ toDigits n
-- end problem 1 solution

-- problem 2 below
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
