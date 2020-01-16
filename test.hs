module Test where

yell :: String -> String
yell a =
  a ++ "!"

lastLetter :: String -> Char
lastLetter sentence =
  sentence !! 4

lastWord :: String -> String
lastWord sentence =
  drop 9 sentence

thirdChar :: String -> Char
thirdChar string =
  string !! 2

letterIndex :: Int -> Char
letterIndex index =
  "Curry is awesome!" !! (index - 1)

reverseString :: [Char] -> [Char]
reverseString [] = []
reverseString (x:xs) = [x] ++ reverseString xs 

rvrs :: String -> String
rvrs string =
  drop 9 string ++ take 4 (drop 5 string) ++ take 5 string

data Mood
  = Blah
  | Woot
  deriving Show

changeMood :: Mood -> Mood
changeMood Blah  = Woot
changeMood _ = Blah

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness
    then putStrLn "eyyyyyy. What's shakin'?"
  else
    putStrLn "pshhhhh."
  where cool v =
          v == "downright frosty yo"

tupFunc :: (Int, [a])
        -> (Int, [a])
        -> (Int, [a])
tupFunc (a, b) (c, d) =
  ((a + c), (b ++ d))

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (x == reverse x)

myAbs :: Integer -> Integer
myAbs x = if x < 0 then x * (-1) else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)
fn xs = w `x` 1
  where w = length xs

fst' :: (a, b) -> a
fst' (a, b) = a

len :: [a] -> Integer
len [] = 0
len (_:xs) = 1 + (len xs)

rvrsStr :: [Char] -> [Char]
rvrsStr (x:xs) = rvrsStr xs ++ [x]
rvrsStr [] = []

--
-- Type Inference
--

t :: Num a => a -> a -> a
t x y = x + y + 3

t2 x y z = x ++ y ++ z

myMult x = (x / 3) * 5
-- :: Fractional a => a -> a

-- take :: Int -> [a] -> [a]
myTake x = take x "hey you"
-- :: Int -> [Char]

myCom x = x > (length [1..10])
-- :: Int -> Bool

myAlph x = x < 'z'
-- :: Char -> Bool

{-# LANGUAGE NoMonomorphismRestriction #-}

f1 = (* 9) 6
f2 = head [(0, "doge"), (1, "kitteh")]
f3 = head [(0 :: Integer, "doge"), (1, "kitteh")]
f4 = if False then True else False
f5 = length [1, 2, 3, 4, 5]
f6 = (length [1, 2, 3, 4]) > (length "TACOCAT") -- :: Integer -> Bool

-- [0] constrained polymorphic
-- [1] fully polymorphic
-- [2] concrete value

-- ff :: zed -> Zed -> Blah
--       [1]    [2]    [2]

-- ff1 :: Enum b => a -> b -> C
--                 [1]  [0]  [2]

-- ff2 :: f -> g -> C
--       [1]  [1]  [2]

--
-- Write type signature
--

foist :: [a] -> a
foist (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y =
  if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

data Trivial' =
  Trivial

instance Eq Trivial' where
  Trivial == Trivial = True

sort :: Ord a => [a] -> [a]
sort = undefined
