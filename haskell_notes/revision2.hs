-- FIXME: 06 Recursive functions

-- todo 6.1 recursive function

-- my version fac :: Num a => a -> a
-- ! compiler fac :: (Eq p, Num p) => p -> p
-- ! why it needs Eq? check for 0
fac 0 = 1
fac n = n * fac (n-1)
-- The recursive definition diverges on integers < 0 because the base case is never reached:
-- *** Exception: stack overflow


-- todo 6.2 why is recursion useful
-- can be proved using the simple but powerful mathematical technique of induction


-- todo 6.3 recursion on lists
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- reverse' :: [a] -> [a]
-- reverse' [] = []
-- reverse' (x:xs) = reverse' xs ++ [x]

-- ! nice
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- remove the first n elements from a list
drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []      = [] -- ! notice this case
drop' a (_:xs) = drop' (a-1) xs

-- ! NOTE append two lists
-- (++) :: [a] -> [a] -> [a]
-- []     ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)


-- todo 6.4 quicksort
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger -- it is [x] not x
  where
    smaller = [a | a <- xs, a < x]
    larger  = [a | a <- xs, a >= x]

-- todo 6.e

-- 6.e.1
-- and' :: [Bool] -> Bool
-- and' [] = True
-- and' (x:xs) = if x == True then True && and' xs
--     else False

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == True  = True && and' xs
            | otherwise  = False

-- ! nice
-- and'' []     = True
-- and'' (x:xs) = x && and'' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n-1) x

-- select nth element in a list
-- * OKAY
(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

-- decide if a value is an element of a list
-- * OKAY
elem' :: Eq a => a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs) | y == x    = True
               | otherwise = False || elem' y xs

-- 6.e.2
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 6.e.3
msort :: Ord a => [a] -> [a]
msort xs | length xs <= 1 = xs
         | otherwise      = msort left `merge` msort right
                              where
                                left = take (length xs `div` 2) xs
                                right = drop (length xs `div` 2) xs

-- FIXME: 07 Higher order functions
-- todo 7.1 higher order funtions
-- a function is called higher-order if it takes a function as an argument
--    or returns a function as a result

twice :: (a -> a) -> a -> a
twice f x = f (f x)
-- twice is higher order because it takes a function as its first argument

-- ! todo 7.2 why are they useful
-- common programming idioms can be encoded as functions within the language itself
-- domain specific languages
-- encapsulation using partial function application
-- algebraic properties

-- todo 7.3 the map function
map' :: (a -> b) -> [a] -> [b]
-- map' (+1) [1,2,3,4]
-- [2,3,4,5]

map' f xs = [f x | x <- xs]

-- map'' f []     = []
-- map'' f (x:xs) = f x : map f xs

-- todo 7.4 the filter function
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

-- ! nice
-- filter p [] = []
-- filter p (x:xs) | p x       = x : filter p xs
--                 | otherwise = filter p xs

-- todo 7.5 the foldr function
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- ! nice
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr f v xs)
{-
The higher-order library function foldr (fold right) encapsulates this simple pattern
of recursion, with the function '+' and the value v as arguments.

sum = foldr (+) 0
product = foldr (*) 1
or = foldr (||) False
and = foldr (&&) True

-- TODO:
However, it is best to think of foldr non-recursively, as simultaneously
replacing each
! (:) in a list by a given function, and
! [] by a given value

-}

-- other foldr examples
-- length'' :: [a] -> Int
-- length'' [] = 0
-- length'' (_:xs) = 1 + length xs

-- length [1,2,3]
-- = length (1:(2:(3:[])))
-- = 1+(1+(1+0))
-- = 3

-- ! Replace each (:) by \_ n -> 1+n and [] by 0.
length'' = foldr' (\_ n -> 1 + n) 0

reverse'' = foldr' (\x xs -> xs ++ [x])

-- FIXME: PPT 06.16 foldr

-- todo 7.6 why is foldr useful
-- simpler
-- can be proved using algebraic properties of foldr, such as fusion and the  banana split rule
-- advanced program optimisations can be simpler if foldr is used in place of explicit recursion


-- todo 7.7 other library functions
-- (.) returns the composition of two function as a single function
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = \x -> f (g x)

odd' :: Int -> Bool
odd' = not . even

-- all
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and [p x | x <- xs]
-- all even [2,4,6,8,10]
-- True


-- any
any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or [p x | x <- xs]
-- ! any (== ’ ’) "abc def"
-- True

-- todo 7.e
-- TODO:


-- FIXME: 08 types and classes
-- todo 8.1 type declarations
-- a new name for an existing type can be defined using a type declaration
type Pos = (Int, Int)
origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1,y)

-- ! like function definitions, type declarations ca also have parameters
type Pair a = (a,a)

mult :: Pair Int -> Int
mult (m,n) = m * n

-- type declarations can be nested
type Trans = Pos -> Pos


-- todo 8.2 data declarations
-- a completely new type can be defined by specifying its values using a data declaration

-- type and constructor names must begin with an upper-case letter

data Answer = Yes | No | Unknown

-- answers :: [Answer]
-- answers = [Yes, No, Unknown]

-- flip' :: Answer -> Answer
-- flip' Yes = No
-- flip' No = Yes
-- flip' Unknown = Unknown

-- ! notice the using of data Maybe a = Nothing | Just a
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)


safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x -- you can also use head xs

-- todo 8.3 recursive types
-- in haskell, new types can be declared in terms of themselves
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n -- ! notice this

-- !  *Main> nat2int (Succ (Succ Zero))
-- !  2

-- ! notice
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- * add
add' :: Nat -> Nat -> Nat
add' m n = int2nat (nat2int m + nat2int n)

add'' :: Nat -> Nat -> Nat
add'' Zero n = n
add'' (Succ m) n = Succ (add'' m n)

-- todo 8.4 arithmetic expressions
data Expr = Val Int
          | Addd Expr Expr
          | Mul Expr Expr

-- Add (Val 1) (Mul (Val 2) (Val 3))

size :: Expr -> Int
size (Val n) = 1
size (Addd x y) = size x + size y
size (Mul x y) = size x * size y

eval :: Expr -> Int
eval (Val n) = n
eval (Addd x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- ? ppt 20

-- todo 8.5 binary trees
data Tree a = Leaf a
            | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)      = x == y
occurs x (Node l y r)  = x == y
                        || occurs x l
                        || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x] -- ! note it has to be wrapped in (), it cannot be Leaf x
flatten (Node l x r) = flatten l ++ [x] ++ flatten r


occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)     = x == y
occurs' x (Node l y r) | x == y    = True
                       | x <  y    = occurs' x l
                       | otherwise = occurs' x r
-- this is more efficient, because it only traverses one path down the tree

-- ? how about this one am i right???
data KVStore k v = KVPair k v ( KVStore k v ) | Empty deriving Show

test = KVPair 1 "hi" (KVPair 2 "hello" (KVPair 3 "there" Empty))
delete :: Eq k => k -> KVStore k v -> KVStore k v
delete key Empty = Empty
delete key (KVPair k v remains) | key == k   = remains
                                | otherwise  = KVPair k v (delete key remains)
                                -- ! dont forgot delete takes two parameters


-- FIXME: Interactive programming

-- todo 10.1
-- interactive programs can be written in haskell by using types to distinguish pure expressions
-- from inpure actions that may  involve side effects

-- IO a
-- the type of actions that return a value of type a

-- IO Char -- return a character
-- IO () -- return no result value

-- ! getChar :: IO Char
-- the action getChar reads a cahracter from the keyboard, returns the character as its result value

-- todo 10.2 sequencing
act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

-- todo 10.3 derived primitives
-- getLine :: IO String
-- getLine = do x <- getChar
--             if x == '\n' then
--               return []
--             else
--               do xs <- getLine
--                 return (x:xs)

-- writing a string to the screen
-- putStr' :: String -> IO ()
-- putStr' []      = return ()
-- putStr' (x:xs)  = do putChar x
--                     putStr' xs


-- FIXME: Lazy evaluation
{-
  1. avoids doing unnecessary evaluation
  2. allows programs to be more modular
  3. allows us to program with infinite lists

  this technique is called lazy evaluation and haskell is called a lazy functional language


---
Evaluating Expressions

Basically, expressions are evaluated or reduced by successively applying definitions until
no further simplification is possible.


---
Reduction Strategies

At each stage during evaluation of an expression there may be many possible subexpressions
that can be reduces by applying a definition

Two common strategies for deciding which redex (reducible expression) to choose:
  1. innermost reduction
  2. outermost reduction


---
Termination

outermost reduction may give a result when the innermost reduction fails to terminate

? ppt lazy_evaluation_p7 for a given expressoin


---
Number of reductions

outmost reduction may require more steps that innermost reduction


---
Thunks
Outmost reduction is inefficient because (3+4) is duplicated when square is reduced
? and thus must be reduced twice. Therefore: use sharing


---
Lazy Evaluation

New evaluation strategy:
Lazy Evaluation = outmost reduction + Sharing

  - lazy evaluation never requires more reduction steps than innermost reduction
  - haskell uses lazy evaluation


---
Infinite Lists
In addition to the termination advantage, using lazy evaluation allows us
to program with infinite lists of values

ones :: [Int]
ones = 1 : ones

  1. Innermost reduction
    infinite

  2. lazy evaluation
    head ones = head (1 : ones)
              = 1


Using lazy evaluation, expressions are only evaluated as much required
to produce the final result


---
Modular programming

Lazy evaluation allows to make programs more modular by separating control from data
? take 5 [1..]
- control data


! By separating the generation of the primes from the constraint of finiteness, we obtain a
modular definition on which different boundary  conditions can be imposed for different solutions.
-- takeWhile (<15) primes
-- [2,3,5,7,11,13]

-}

primes :: [Int]
primes = sieve [2..]

-- ???
sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]










