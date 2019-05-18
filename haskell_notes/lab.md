# Lab 01

```hs
1 :: Num a => a
```

means that `1` has some type `a`, where `a` is an instance of the `Num` typeclass.
`Num` is not a type, but a typeclass, which describes common properties of various types.
The `Num` typeclass, for example, describes types that are numeric, and so support basic arithmetic.

---
2.6

```hs
Prelude> :t sqrt
sqrt :: Floating a => a -> a
Prelude> :t (+1) . sqrt
(+1) . sqrt :: Floating c => c -> c
```

---
2.5

```hs
Prelude> :t (+)
(+) :: Num a => a -> a -> a
Prelude> :t (+1)
(+1) :: Num a => a -> a
```

---
2.7

```hs
-- this is from ppt
all :: (a -> Bool) -> [a] -> Bool
all p xs = and [p x | x <- xs]

-- from exercises in terminal
Prelude> :t all
all :: Foldable t => (a -> Bool) -> t a -> Bool
Prelude> :t all even
all even :: (Foldable t, Integral a) => t a -> Bool
```

---
Note that there is a difference between `->` and `=>` . The first is used for the function signature; the second is used to show type classes.

```hs
Prelude> :t [[1,2],[3,4]]
[[1,2],[3,4]] :: Num a => [[a]]
```

---
3.12

```hs
Prelude> three x = (x,x,x)
Prelude> :t three
three :: c -> (c, c, c)
```

---

```hs
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
```

```hs
-- median [] == exception
-- median [1] == 1
-- median [1,2] == 1.5
median :: Fractional a => [a] -> a
median xs
    | odd n     = idxElem
    | otherwise = (idxElem + idxPrevElem) / 2
  where
    n   = length xs
    idx = n `div` 2

    idxElem     = xs !! idx
    idxPrevElem = xs !! (idx - 1)
```

## lab 2

```hs
*Main> :t (\x -> x * 2)
(\x -> x * 2) :: Num a => a -> a
*Main> (\x -> x * 2) 8
16
```

---

```hs
*Main> :t (/)
(/) :: Fractional a => a -> a -> a
*Main> 5 / 12
0.4166666666666667
```

---
[Polymorphic guesswork](https://en.wikibooks.org/wiki/Haskell/Type_basics_II)

```hs
Prelude> (-7) + 5.12
-1.88
```

This may seem to add two numbers of different types – an integer and a non-integer. Let's see what the types of the numbers we entered actually are:

```hs
Prelude> :t (-7)
(-7) :: (Num a) => a
```

So, `(-7)` is neither `Int` nor `Integer`! Rather, it is a _polymorphic_ value, which can "morph" into any number type. Now, let's look at the other number:

```hs
Prelude> :t 5.12
5.12 :: (Fractional t) => t
```

`5.12` is also a polymorphic value, but one of the `Fractional` class, which is a subset of Num (every `Fractional` is a `Num`, but not every `Num` is a `Fractional`; for instance, `Int`s and `Integers` are not `Fractional`).

When a Haskell program evaluates `(-7) + 5.12`, it must settle for an actual matching type for the numbers. With no other restrictions, `5.12` will assume the default `Fractional` type of `Double`, so `(-7)` will become a `Double` as well. Addition then proceeds normally and returns a `Double`.

---
Integral

```hs
*Main> :t 4 `div` 3
4 `div` 3 :: Integral a => a
```

---
notice the difference, `zip` required two `[a]`

```hs
-- one
*Main> :t zip [True, True] [False, True]
zip [True, True] [False, True] :: [(Bool, Bool)]
-- two
*Main> :t zip [True, True]
zip [True, True] :: [b] -> [(Bool, b)]
```

```hs
[ x + 1 | x <- [2.0,-3.5,8.4] ] :: Fractional a => [a]
-- Reason: Fractional (and not Num) because we have floating point values
```

---
***Euclid's algorithm*** to find the greatest common factor

```hs
euclid :: Int -> Int -> Int
euclid x y
  | x == y     = x
  | x < y      = euclid x (y - x)
  | otherwise  = euclid (x - y) y
```

---
2.6  Explicit Recursion 2: Number replacement

```hs
-- my solution
intrep :: [Int] -> Int -> Int -> [Int]
intrep [] a b = []
intrep (x:xs) a b
  | x == a    = [b] ++ intrep xs a b
  | otherwise = [x] ++ intrep xs a b

-- answer
intrep :: [Int] -> Int -> Int -> [Int]
intrep [] _ _ = [] -- nice trick
intrep (x:xs) from to -- semantic naming
  | x == from   = to : intrep xs from to
  | otherwise   = x : intrep xs from to
```

---
2.8

## Review

```hs
isSeparator' :: Char -> Bool
isSeparator' c = c `elem` " \n\t,;:.?!"

breakTextHelper :: String -> String -> [String]
breakTextHelper [] acc  = [acc]
breakTextHelper (c:cs) acc
    | isSeparator' c    = acc : breakTextHelper cs "" -- or [acc] ++
    | otherwise         = breakTextHelper cs (acc ++ [c])

breakText :: String -> [String]
breakText cs = breakTextHelper cs ""
```

## lab 03

3.1 recursive addition

```hs
--recursive addition
addition :: Integer -> Integer -> Integer
addition 0 0 = 0
addition 0 y = 1 + addition 0 (y-1)
addition x y = 1 + addition (x-1) y
```

---

3.2 recursive multiplication

```hs
mult :: Integer -> Integer -> Integer
mult 0 _ = 0
mult x y = addition y (mult (x-1) y)

-- using foldr
multFold :: Integer -> Integer -> Integer

-- this one is easier to understand
multFold x y = foldr (\_ acc = addition y acc) 0 [1..x]

-- use (.)
multFold x y = foldr (addition . const y) 0 [1..x] -- addition definied 3.1

-- use replicate and from Integral
multFold x y = foldr addition 0 (replicate (fromIntegral x) y)
```

---

3.3 recursive exponential

```hs
power :: Integer -> Integer -> Integer
power _ 0 = 1
power x y = mult x (power x (y-1))

-- using foldr
power x y = foldr (\_ acc = mult x acc) 1 [1..y]

-- using (.) and foldr
power x y = foldr (mult . const x) 1 [1..y]
```

```hs
-- power of 2
powerof2 :: Integer -> [Integer]
powerof2 n = [power 2 x <- [0..n]]

-- power of 2 using foldr
powerof2Fold n = foldr (\x acc -> power 2 x : acc) [] [0..n]

-- power of 2 using map
powerof2Map n = map (power 2) [0..n]
```

---

3.4 recursive division

***NEED REVIEW***

```hs
-- recursive division
division :: Integer -> Integer -> Integer
division _ 0 = "division by zero"
division x y
  | x < y     = 0
  | otherwise = 1 + division (x-y) y

-- TODO: using foldr
divisionFold :: Integer -> Integer -> Integer
```

---

3.5
implement a higher order quicksort

```hs
-- quicksort implementation from Chapter 6
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- higher order quicksort
qsort' :: (a -> a -> Bool) -> [a] -> [a]
qsort' _ [] = []
qsort p (x:xs) = qsort p smaller ++ [x] ++ qsort p larger
  where
    smaller = [a | a <- xs, p a x]
    larger = [b | b <- xs, not $ p a x] -- not (q a x)
```

---

TODO: Advanced higher order functions
