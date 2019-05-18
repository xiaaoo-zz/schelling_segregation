import System.Random
-- fp__02__11
-- function application is assumed to have higher priroirty than all other operators
double x = x + x
quadruple x = double (double x)


factorial n = product [1..n]
average ns = sum ns `div` length ns
-- * x `f` y is just syntactic sugar for f x y.

n = a `div` length ns
  where
    a = 5
    ns = [1,2,3,4,5]

-- TODO: fp_03
-- all type errors are found in compile time, which makes programs much **fater and safer** by removing the need for type chekcs at run time
-- * 03__06 basic types:
-- Bool Char String Int Integer(arbitrary precision) Float

-- List types
-- a list type is sequence of values of the same type
-- the type of the elements of the list is unrestricted, for example, we could have a list of list
-- [['a'],['b','c']] :: [[Char]]


-- Tuple types
-- a type is a sequence of valus of different types
-- (False, 'a', True) :: (Bool, Char, Bool)
-- the type of tuple encodes its size

-- function types
-- a fucntion is a mapping from values of one type to values of another type


-- * 03__13 Curried functions
-- functions with multiple argumenst are also possible by returning functions as a result
-- mult x y z means ((mult x) y) z
-- Unless tupling is explicitly required, all functions in Haskell are normally defined in curried form.


-- 03__19 polumorphic functions
-- a function is called polymorphic if its type contains one or more type variables
-- length :: [a] -> Int
-- type variables must begin with a lower-case letter, and are usually names a, b, c etc.

-- 03__22 overloaded functions
-- a polymorphic funciton is called overloaded if its type contains one or more class constrainsts
-- * (+) :: Num a => a -> a -> a
-- for any numeric type a, (+) takes two values of type a and returen a value of type a
-- * constrained type variables can be instantiated to any tpes that satisfy the constraints

-- * 03__24 Haskell has a number of type classes
-- Num: Numeric types
-- Eq: Equality types
-- Ord: Ordered types

-- TODO: 03 exercises
-- !! note the following two different versions
-- [(False,'0'),(True,'1')] :: [(Bool, Char)]
-- ([False,True],['0','1']) :: ([Bool], [Char])
-- [tail,init,reverse] :: [[a] -> [a]]

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

-- ! NOTE THIS
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

-- ?? why
twice :: (t -> t) -> t -> t
twice f x = f (f x)



-- TODO: defining functions
-- conditional expressions
my_abs :: Int -> Int
my_abs n = if n >= 0 then n else -n
-- 04__03 conditional expression must alwasy have an else branch


-- * Guarded Equations
-- as an alternative to conditionals, function can also be defined using guarded equations
signum :: Int -> Int
signum n | n > 0       = 1
         | n == 0      = 0
         | otherwise   = -1
-- the catch all condition otherwise is defined in the prelude by otherwise = True


-- 04__06 Pattern Matching
-- many functions have a prticularly clear definition using pattern matching ontheri arguments
(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False

-- ! however the following definition is more efficient
-- because it avoids evaluating the second argument if the first arguent is False
-- * True  && b = b
-- * Flase && _ = Flase
-- ! patterns may not repeat vairables: for example: b && b = b will give an error


-- List Patterns
-- functions on lists can be defined using x:xs patterns
head' :: [a] -> a
head' (x:_) = x
-- x:xs patterns only match non-empty lists
-- x:xs patterns must be parenthesised, because application has priority over (:)


-- 04__12__14 Lambda Expressions
-- function can be constructed without naming the functions by using lambda expression
add' x y = x + y
add'' = \x -> (\y -> x + y)
-- ! 04__16
-- * 04__17 lambda expressions can be used to avoid naming functions that are only referneced once
odds' n = map (\x -> x*2 + 1) [0..n-1]


-- Operator Sections
-- an operator written between its two argumengs can be cvonverted into a curried function written before its two arguments by using parentheses
-- (+) 1 2
-- this convention also allows one of the arguemnts of the operator to be included in the parentheses
-- (+1) 3
-- ! (1/) reciprocation function
-- * (1+) successor function
-- * (/2) halving function


-- ? i didnt do exercise for 04 yet


-- TODO: 05 List Comprehensions
-- lists comprehensions
-- [x^2 | x <- [1..5]]
-- the expression x <- [1..5] is called a generator
-- ! changing the order of the generators changes the order of the lements in the final list
-- [(x,y) | y ← [4,5], x ← [1,2,3]]
-- [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]
-- multiple generators are like nested loops

-- Dependant Generators
-- [(x,y) | x ← [1..3], y ← [x..3]]
-- * nice
concat :: [[a]] -> [a]
concat xss=[x | xs <- xss, x <- xs]


-- Guards
-- list comprehensions can use guards to restirct the values produced by earlier generators
-- [x | x <- [1..10], even x]
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]


-- The Zip Function
-- a useful library function is zip, which maps atwo lists to a list of pairs of their correspounding elements

-- ! note tail xs has to be wrapped in parentheses
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- ! nice
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x' == x]


-- * String Comprehensions
-- a string is a sequence of characters enclosed in double quotes, internally, however, strings are represented as lists of characters
-- * -> becuase strings are just special kinds of lists, any polymorphic functions that operatees on lists can also be applied to strings
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x' == x]

-- TODO: didn't do the exercises


-- TODO: Recursive Functions
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length xs

-- * Multiple Arguments
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]


-- TODO: didn't do the exercises for 06


-- TODO: QA
e5 :: Num a => a -> a
e5 n = n * 2

-- ! the following will give an error
-- e6 :: Num a => a -> b
-- e6 n = n * 2
-- because the input and output are still the same type, we use a -> a not a -> b

-- mult' :: Int -> Int -> Int
mult' x y = x * y
-- * if you check :type mult', it will give Num a => a -> a -> a
-- becuase a is more general and works not only for Int but for all numeric types, Integer, Float, Double

mkString = "hello"
-- :type [Char], because String is simply an alias for [Char]

-- [1,2,3,4,5] is syntactic sugar for 1:2:3:4:5:[]


-- TODO: Higher-Order Functions
-- a function is called higher-order function if it takes a function as an argument or returen a funtion as a result
twice' :: (a -> a) -> a -> a
twice' f x = f (f x)

-- * 07__05 The Map Function
-- map' :: (a -> b) -> [a] -> [b]
-- ! the map function can be defined using list comprehensions
-- map f xs = [f x | x <- xs]
-- ! alternatively, using recursions
-- map f [] = []
-- map f (x:xs) = f x : map f xs


-- 07__06 The Filter function
-- filter :: (a -> Bool) -> [a] -> [a]
-- * using list comprehensions
-- filter f xs = [x | x <- xs, f x]
-- * using recursions (Guarded Equations)
filter' f [] = []
filter' f (x:xs)
  | f x        = x : filter' f xs
  | otherwise  = filter' f xs


-- The Foldr function
-- sum = foldr (+) 0
-- or = foldr (||) False
-- ! note foldr
-- foldr' :: (a -> b -> b) -> b -> [a] -> b
-- ??? why ???
-- :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr'' f v [] = v
foldr'' f v (x:xs) = f x (foldr f v xs)
-- ! 07__12

length'' = foldr'' (\_ n -> 1+n) 0
-- FIXME: PLEASE REVIEW
-- ! 07__15 07__16
-- ! 07__17
reverse''' = foldr'' (\x xs -> xs ++ [x]) []


-- Other Library Functions
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = \x -> f (g x)

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and [p x | x <- xs]
-- all' even [2,4,6,8]
-- dually, the library function any decides if at least one elemnt of a list satisfies a predicate
any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or [p x | x <- xs]
-- any (== ' ') "abc def"

-- ! review 07__22
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

-- dropWhile


-- TODO: REVIEW 07__25
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

addUncurried :: (Int,Int) -> Int
addUncurried (x,y) = x+y
-- addUncurried (1,2)
-- curry adduncurried 1 2
-- ! :t curry addUncurried :: Int -> Int -> Int


-- todo: exercise for 07



-- TODO: type classes
-- please review


-- TODO: Declaring types and classes
type Pos = (Int,Int)
origin :: Pos
origin = (0,0)

--- like funtion definitions, type declarations can also have parameters
type Pair a = (a,a)
mult''' :: Pair Int -> Int
mult''' (m,n) = m*n

copy :: a -> Pair a
copy x = (x,x)


-- Type declarations can be nested
type Trans = Pos -> Pos

-- however, they cannot be recursive
-- WRONG: type Tree = (Int,[Tree])


-- ! 08__05 Data Declarations
-- a completely new type can be defined by specifying its values using a data declaration
data Answer = Yes | No | Unknown
-- the three values are called constructors for the type Answer
-- type and constructor names must always begin with an upper-case letter

answers :: [Answer]
answers = [Yes,No,Unknown]

flip' :: Answer -> Answer
flip' Yes = No
flip' No = Yes
flip' Unknown = Unknown


-- the constructors in a data decalration can also have parameters
data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x*y
-- Circle and Rect can be viewed as functions that construct values of type Shape


-- data decalrations themeselves can also have parameters
data Maybe' a = Nothing' | Just' a

safediv :: Int -> Int -> Maybe' Int
safediv _ 0 = Nothing'
safediv m n = Just' (m `div` n)

safehead :: [a] -> Maybe' a
safehead [] = Nothing'
safehead xs = Just' (head xs)

-- Recursive Types
-- !? note: type decalration cannot be recursive, but data declarations can
data Nat = Zero | Succ Nat
-- Nat is a new type, with constructors Zero :: Nat and Succ :: Nat -> Nat
-- Zero
-- Succ Zero
-- Succ (Succ Zero)


-- Succ (Succ (Succ Zero))
-- can represents the natural number
-- 1 + (1 + (1 + 0)) = 3


nat2int :: Nat -> Int
nat2int Zero    = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- ? 08_15__16


-- Arithmetic Expressions
-- ? 08_17__19__20

-- ! binary tree
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- !? why the type is this
-- this one is not efficient
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)      = x == y
occurs x (Node l y r)  = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r
-- this is more efficient
occurs' x (Leaf y)      = x == y
occurs' x (Node l y r)  | x == y = True
                        | x < y  = occurs x l
                        | x > y  = occurs x r

-- todo exercises for 08


-- todo exercises of lab 01
quadratic :: (Floating a, Ord a) => a -> a -> a -> [a]
quadratic a b c 
    | a == 0    = error "Quadratic equation undefined for a = 0 (its not a quadratic equation anymore)!"
    | disc < 0  = [] -- discriminant negative: no solution in the reals
    | disc == 0 = [-(b / a2)] -- discriminant zero => 1 root
    | otherwise = [((-b) + sqd) / a2, ((-b) - sqd) / a2]-- discriminant positive => 2 roots
  where
    disc = b*b - 4*a*c
    sqd = sqrt disc
    a2 = 2*a


-- todo exercises of lab 02
-- 2.6
intrep :: [Int] -> Int -> Int -> [Int]
intrep [] a b = []
intrep (x:xs) a b
  | x == a    = [b] ++ intrep xs a b
  | otherwise = [x] ++ intrep xs a b

-- 2.7
listrep :: Eq a => [a] -> a -> a -> [a]
listrep [] _ _ = []
listrep (x:xs) from to
    | x == from  = to : listrep xs from to
    | otherwise  = x : listrep xs from to

-- 2.8
-- whitespace (space, newline, tab);
-- comma, semi-colon, colon;
-- fullstop, question mark, exclamation mark.

-- input ['a', 'b', 'c', ',', ' ', 'e', 'f']
-- input "abc, ef"
-- output [['a','b','c'], ['e', 'f']]
-- output ["abc", "ef"]
-- makeWord :: Char -> String
-- makeWord
isSeparator' :: Char -> Bool
isSeparator' c = c `elem` " \n\t,;:.?!"

breakTextHelper :: String -> String -> [String]
breakTextHelper [] acc  = [acc]
breakTextHelper (c:cs) acc
    | isSeparator' c    = acc : breakTextHelper cs "" -- or [acc] ++
    | otherwise         = breakTextHelper cs (acc ++ [c])

breakText :: String -> [String]
breakText cs = breakTextHelper cs ""

foo :: IO Int
foo = do
   r1 <- getStdGen
   let (x, r2) = randomR (0,2) r1
   setStdGen r2
   return x

seed::Int
seed = 40

giveList :: [Int]
giveList = [8,9,4,5,2]

generator = mkStdGen seed

giveRandomElement :: Int
giveRandomElement =
  giveList !! rand
    where
      n = length giveList
      (rand, g2) = randomR (0,(n-1)) generator

atRandIndex :: [a] -> IO a  -- note that this is gives itself an IO action
atRandIndex l = do
    i <- randomRIO (0, length l - 1)
    return $ l !! i
