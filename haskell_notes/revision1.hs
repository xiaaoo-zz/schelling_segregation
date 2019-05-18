-- FIXME: lecture 2

double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns


-- n = a `div` length xs where
--   a = 10
--  xs = [1,2,3,4,5] // compile error, as xs and a is not in the same line

n = a `div` length xs where
  a = 10
  xs = [1,2,3,4,5]

-- todo L02.3 implement last
-- select the last element
last' :: [a] -> a
last' xs = reverse xs !! 0
-- last' xs = head (reverse xs) -- used function head
last'' xs = xs !! (length xs - 1)

-- todo L02.5 implement init
-- drop the last element
init' :: [a] -> [a]
init' xs = take (length xs - 1) xs
init'' xs = reverse (tail (reverse xs))



-- FIXME: lecture 3
-- curried function: takes arguments one at a time
add' :: Num a => a -> a -> a
add' x y = x + y

-- todo polymorphic functions
-- a function is called polymorphic if its type contains one or more type variables
-- type variable must begin with a lower-case letter, usually named a, b, c, etc.

{-
-- todo overloaded functions

-- todo class constrains
a polymorphic function is called overloaded if its type containes one or more class constrains

  (+) :: Num a => a -> a -> a

  for any numeric type a, (+) takes two values of type a and returns a value of type a

  >1+2
  3

  > 1.0 + 2.0
  3.0

  > ’a’ + ’b’
  ERROR

type classes
- Num
- Eq
- Ord
-}

-- todo L03.02
{-
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

-- * nice
double :: Num a => a -> a
double x = x*2

-- ! you have to add Eq
-- my wrong version palindrome [a] -> Bool
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

-- ! NOTE
-- compiler twice :: (t -> t) -> t -> t
-- myAnswer twice :: (a -> a) -> a -> a
twice f x = f (f x)

-}


-- FIXME: lecture 3

-- todo 3.1 conditional expressions
abs' :: (Num a, Ord a) => a -> a
abs' n = if n >= 0 then n else -n
-- must have an else

-- todo 3.2 an alternative to conditionals, guarded equations
-- involving multiple conditions easier to read
-- otherwise is defined in the prelude by otherwise = True
abs'' :: (Num a, Ord a) => a -> a
abs'' n | n >= 0       = n
        | otherwise    = -n

-- todo 3.3 pattern matching
(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False

-- however, the folliwing is more efficient
-- True  && b  = b
-- False && _  = False
-- the underscore symbol is a wildcard pattern taht matches any argument value

-- todo 3.4 list patterns
-- every non-empty list is constructed by repeated use of an operator (:)
-- called “cons” that adds an element to the start of a list.

-- 1:(2:(3:(4:[])))
-- Main> 1:3:4:[]
-- [1,3,4]

-- function on lists
headd :: [a] -> a
headd (x:_) = x

taill :: [a] -> [a]
taill (_:xs) = xs

-- head and tail map any non-empty list to its first and remaining elements
-- ! x:xs patterns only match non-empty lists
-- ! x:xs patterns must be parenthesised, as application has priority over (:)

-- todo 3.5 lambda expression
-- function can be constructed without naming the functions by using lambda expression
-- backslash \x -> x + x

-- * 3.5.1 useful - can be used to give a formal meaning to functions defined using curring
-- add x y = x + y
-- add = \x -> (\y -> x + y)

-- * 3.5.2 useful - when defining functions that return functions as results
-- const :: a -> b -> a
-- const x _ = x

-- can be defined by
const' :: a -> (b -> a)
const' x = \_ -> x

-- * 3.5.3 useful - can be used to avoid naming functions that are only referenced once
-- !
odds n = map f [0..n-1]
          where
            f x = x * 2 + 1
-- can be simplified to
odds' n = map (\x -> x * 2 + 1) [0..n-1]

-- todo 3.6 operator sections
-- An operator written between its two arguments can be converted into a curried function written before its two arguments by using parentheses.
-- (+) 1 2
-- 3

-- (1+) 2
-- 3

-- (+2) 1
-- 3

-- in general, if + is an operator then functions of the form (+), (x+), (+y)
-- * are called sections

-- why are sections useful
-- * 3.6.1
-- useful function can sometimes be constructed in a simple way using sections
-- (1+) successor function
-- (1/) reciprocation function
-- (*2) doubling function
-- (/2) halving function

-- todo 3.e exercies
-- 3.e.1 conditional expression
-- * null :: [a] → Bool
safetail' :: [a] -> [a]
safetail' xs = if null xs then [] else tail xs

-- 3.e.1 guarded equations
safetail'' xs | null xs   = []
              | otherwise = tail xs

-- 3.e.1 pattern matching
safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs


-- 3.e.2 logical or operator (||)
(|||) :: Bool -> Bool -> Bool
False ||| False  = False
_     ||| _      = True

-- ? todo 3.e.3

-- ? todo 3.e.4


-- FIXME: 05

-- todo 5.1 lists comprehensions
-- to construct new lists from old lists
-- [x^2 | x <- [1..5]]
-- the expression x <- [1..5] is called a generator

-- todo 5.2 dependant generators
-- [(x,y) | x <- [1,2,3], y <- [4,5]]
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- multiple generators are like nestsed loops

-- using a dependant generator -> concatenate a list of lists
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

-- concat [[1,2,3],[4,5],[6]]
-- [1,2,3,4,5,6]

-- todo 5.3 guards
-- list comprehensions can use guards to restrict the values produced by earlier generators
-- [x | x <- [1..10], even x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

-- todo 5.4 the zip function
-- zip :: [a] -> [b] -> [(a,b)]

-- pairs of adjacent elements from a list
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- ! sorted ! Ord
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- ! Eq ! zip xs [0..] to get the index
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']


-- todo 5.5 string comprehensions
-- a string is a sequence of characters enclosed in double quotes
-- strings are represented as lists of characters
-- any polymorphic function that operates on lists can also be applied to strings

-- take 3 "abcde"
-- "abc"

-- zip "abc" [1,2,3,4]
-- [('a',1),('b',2),('c',3)]

-- ! nice, list comprehensions on strings
count :: Char -> [Char] -> Int
count x xs = length [x' | x' <- xs, x' == x]

-- todo 5.e

-- 5.e.2

factors' :: Int -> [Int]
factors' n = [x | x <- [1..n-1], n `mod` x == 0]

perfect :: Int -> Bool
perfect n = sum (factors' n) == n

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x] -- !! 不是perfect n

-- 5.e.2
scalar :: Num a => [a] -> [a] -> a
scalar as bs = sum [a*b | (a,b) <- zip as bs]
