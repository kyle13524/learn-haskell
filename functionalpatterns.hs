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

