-- 7.4 Pattern Matching

data WherePenguinsLive =
  Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

-- Destructure type values using pattern matching
gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereItLives) =
  whereItLives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

-- Unpack Penguin type and pattern match on the value
galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng    Antarctica) = True
antarcticPenguin _                    = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
  (galapagosPenguin p)
  || (antarcticPenguin p)


-- These next two implementations are the same.
-- One using functions, the other using pattern matching.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

f' :: (a, b) -> (c, d) -> ((b, d), (a, c))
f' (a, b) (c, d) = ((b, d), (a, c))

-- Pattern matching on Tuples

-- (+) is a -> a -> a
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-- Equal to addEmUp2 but not a pattern match
addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

-- Fst using pattern matching
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- Third using pattern matching
third3 :: (a, b, c) -> c
third3 (_, _, x) = x


-- 7.5 Case Expressions


-- Func is equivalent to Func'
func x = if x + 1 == 1 then "Awesome" else "wut"

func' x =
  case x + 1 == 1 of
    True -> "Awesome"
    False -> "wut"

-- Palindromes using case expression
pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

-- Palindrome using case expression - using where if we want to reuse y
pal' xs =
  case y of
    True -> "yes " ++ rvs
    False -> "no"
  where rvs = reverse xs
        y = xs == rvs

-- First custom case statement written by Kyle J ;)

-- Text field can either be full or empty. 
data TextField =
  Filled String
  | Empty
  deriving Show

-- Fill text field with reversed string if not already filled. Otherwise, keep it the same
fillTextField :: String -> TextField -> TextField
fillTextField s tf =
  case tf of
    Filled _ -> tf
    Empty -> Filled rvs
  where rvs = reverse s

-- Back to the lesson ;)

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True ->
      putStrLn "eyyyyy. What's shakin'?"
    False ->
      putStrLn "pssssh. Get outta here."
  where cool =
          coolness == "downright frosty yo"


-- Exercises

functionC x y = if (x > y) then x else y

functionC' x y =
  case x > y of
    True -> x
    False -> y

functionC'' x y =
  case gt of
    True -> x
    False -> y
  where gt = x > y

ifEvenAdd2 n = if even n then (n + 2) else n

ifEvenAdd2' n =
  case even n of
    True ->
      n + 2
    False ->
      n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1


-- 7.6 Higher Order Functions

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \ x y -> f y x

-- What makes flip a higher-order function?

-- flip :: (a -> b -> c) -> b -> a -> c
--         [     1     ]
-- flip f x y = f y x
--     [2]     [3]

-- 1. Nest the function argument in parenthesis
-- 2. The argument f is the function a -> b -> c
-- 3. We apply f to x and y. flip will apply f to y and then x

-- Associative parenthesis
-- Since the parameters are right associative, returnLast is the same as returnLast'
returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

-- Parameters are NOT left associative, this will not work. Expecting one parameter and getting four.
-- returnBroke :: (((a -> b) -> c) -> d) -> d
-- returnBroke _ _ _ d = d

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a

-- We want HOFs to manipulate how functions are applied to arguments. Take compare as an example.

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++
  " is the boss of " ++
  show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
  case compare e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither emplooyee\
                   \ is the boss"
    LT -> (flip reportBoss) e e'

-- employeeRank function now accepts a function argument. As long as it takes two employees and returns Ordering, it can be applied.
employeeRank' :: (Employee
                 -> Employee
                 -> Ordering)
                 -> Employee
                 -> Employee
                 -> IO (
                       ) 
employeeRank' f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee\
                   \ is the boss"
    LT -> (flip reportBoss) e e' -- Why rewrite the function when we can just flip the arguments? :D

-- Prelude> employeeRank compare Veep CEO
-- Prelude> CEO is the boss of Veep

-- We can now switch out the compare function with another function that matches the same pattern.
-- CODERS UNITE!

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' =
  compare e e'

-- Prelude> employeeRank codersRuleCEOsDrool Coder CEO
-- Prelude> Coder is the boss of CEO
-- Take THAT CEO, we OWN YOU ;D



-- 7.8 Function Composition



-- (.) :: (B -> c) -> (a -> b) -> a -> c
--           [1]         [2]     [3]  [4]

-- 1. Function from b to c, passed as an argument
-- 2. Function from a to b
-- 3. Value of type a
-- 4. Value of type c

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
--           [1]        [2]          [3]

-- 1. Given a function from b to c
-- 2. Given a function from a to b
-- 3. Return a function from a to c

-- Composed functions:
-- (f . g) x = f (g x)

-- (.) = composition operator
-- f corresponds to (b -> c) function, g corresponds to (a -> b) function
-- g function is applied to polymorphic x to return (x -> b) which is then passed
-- as an argument to (b -> c) which produces the final result

-- Example:
xs = [1, 2, 3, 4, 5]
funcComp = negate . sum $ xs

-- Function application operator has higher precedence over composition operator, meaning the
-- application would occur before the composition, causing a failure. We use $ to get around this.

take5 = take 5 . reverse $ [1..10]
take5enum = take 5 (enumFrom 3)
take5enum' = take 5 . enumFrom $ 3
take5enum'' x = take 5 . enumFrom $ x
take5enumOdd x = take 5 . filter odd . enumFrom $ x
take5enumOddReverse x = filter odd . take 5 . enumFrom $ x


-- 7.9 Point-free style


-- Point-free style is often cleaner as it allows us to compose functions without specifying arguments
-- (f . g . h) x is easier to read than f (g (h x))

-- Example:
-- pf = negate . sum

-- These two functions are equivalent. The second is point-free.

pf2 z xs = foldr (+) z xs

pf2' :: (Foldable t, Num b) => b -> t b -> b
pf2' = foldr (+)

-- Examples:

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)


-- 7.10 Demonstrating composition

print' :: Show a => a -> IO ()
print' a = putStrLn (show a)

print'' :: Show a => a -> IO ()
print'' a = putStrLn . show $ a

-- putStrLn :: String -> IO ()
--               [1]      [2]

-- show :: Show a => a -> String
--               [3]       [4]

-- putStrLn . show :: Show a => a -> IO ()
--                     [5]            [6]

-- (.) :: (b -> c) -> (a -> b) -> a -> c
--        [1]  [2]    [3]  [4]   [5]  [6]

-- If we replace the variables with the specific types we get:
-- (.) :: Show a => (String -> IO ())
--                  -> (a -> String)
--                  -> a -> IO ()

-- 1. The string that putStrLn accepts as an argument
-- 2. The IO () that putStrLn returns, performing the side effect of printing and returning
-- 3. a that must implement the Show type class (Show a => a from show function)
-- 4. The string that show returns
-- 5. The Show a => a the final function expects
-- 6. the IO () the final composed function returns

-- point-free version of print:
print''' :: Show a => a -> IO ()
print''' = putStrLn . show


-- 7.11 Chapter Exercises

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

-- tensDigit' :: Integral a => a -> a
-- tensDigit' x = d
--   where dm =
--         d = snd dm

-- hundredsDigit :: Integral a => a -> a
-- hundredsDigit x = d
--   where dm = x `divMod` 100
--           d = snd dm

foldBool :: a -> a -> Bool -> a
foldBool =
  error
  "Error: Need to implement foldBool!"

foldBool' :: a -> a -> Bool -> a
foldBool' x _ False = x
foldBool' _ y True = y

-- foldBoolC :: a -> a -> Bool -> a

foldBool'' :: a -> a -> Bool -> a
foldBool'' x y b =
  case b of
    True -> y
    False -> x

foldBoolG :: a -> a -> Bool -> a
foldBoolG x y b
  | b == True = y
  | otherwise = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
  print (roundTrip 4)
  print (id 4)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTripAb :: (Show a, Read b) => a -> b
roundTripAb = read . show
