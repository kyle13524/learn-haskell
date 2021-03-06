module Recursion where

import Data.List (intersperse)


-- 8.2 Factorial

fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1 (n - 1)

fixedFactorial :: Integer -> Integer
fixedFactorial 0 = 1
fixedFactorial n = n * fixedFactorial (n - 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n =
  n
incTimes times n =
  1 + (incTimes (times - 1) n)

incTimes' :: (Eq a, Num a) => a -> a
incTimes' 0 = 0
incTimes' times =
  1 + (incTimes' (times - 1))

applyTimes :: (Eq a, Num a) =>
           a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n - 1) f b)


-- 8.3 Bottom


bottom :: Bool -> Int
-- bottom True = error "blah"
bottom False = 0


-- 8.4 Fibonacci Numbers


fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)


-- 8.5 Integral Division

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy n d = go n d 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- 8.6 Chapter Exercises

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

mySum :: (Eq a, Num a) => a -> a
mySum 0 = 0
mySum a = a + mySum(a - 1)

myMultiplication :: (Integral a) => a -> a -> a
myMultiplication a b = go a b 0
  where go a b prod
          | b == 0 = prod
          | otherwise = go a (b - 1) (prod + a)

mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | n <= 100 = mc91 $ mc91 $ n + 11


-- Numbers into words

digitToWord :: Int -> String
digitToWord n =
  case n of
    0 -> "Zero"
    1 -> "One"
    2 -> "Two"
    3 -> "Three"
    4 -> "Four"
    5 -> "Five"
    6 -> "Six"
    7 -> "Seven"
    8 -> "Eight"
    9 -> "Nine"
    _ -> "NaN"

digits :: Int -> [Int]
digits num = go num []
  where go num list
          | num == 0 = list
          | otherwise = go (div num 10) (mod num 10 : list)

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord $ digits n
