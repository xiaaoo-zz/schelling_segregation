-- * Euclid
euclid :: Int -> Int -> Int
euclid x y 
  | x == y    = x
  | x < y     = euclid x (y - x)
  | otherwise = euclid (x - y) y

-- 2.6 2.7
listrep :: Eq a => [a] -> a -> a -> [a]
listrep [] _ _ = []
listrep (x:xs) a b | x == a     = b : listrep xs a b
                  | otherwise  = x : listrep xs a b

-- ? 2.8 Text analysis

-- todo lab 3
-- 3.1
addition :: Integer -> Integer -> Integer
addition 0 0 = 0
addition 0 y = 1 + addition 0 (y-1)
addition x y = 1 + addition (x-1) y

-- 3.2
mult :: Integer -> Integer -> Integer
mult 0 _ = 0
mult x y = addition y (mult (x-1) y)

-- multFold x y = foldr (addition . const y) 0 [1..x]
-- ! below
multFold x y = foldr (\_ acc -> addition y acc) 0 [1..x]

-- todo lab 4
data KVStore k v = KVPair k v (KVStore k v)
                    | Empty
                    deriving Show

kvsExpr :: KVStore Int String
kvsExpr = KVPair 1 "Hello" (KVPair 2 "There" Empty)

-- ! note
-- * if the key already exists
kvsInsert :: Eq k => KVStore k v -> k -> v -> KVStore k v
kvsInsert Empty k v = KVPair k v Empty
kvsInsert (KVPair k' v' kvs) k v 
    | k == k'   = KVPair k' v' kvs
    | otherwise = KVPair k' v' (kvsInsert kvs k v)


data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
-- test tree: unsorted
tree :: Num a => Tree a
tree = Node (Node (Leaf 2) 7
                  (Node (Leaf 5) 6 (Leaf 11))) 2
            (Node (Leaf 1) 5 (Leaf 9))

-- test tree: sorted
sortedTree :: Tree Int
sortedTree = Node (Node (Leaf 3) 3 (Leaf 600)) 5
                  (Node (Leaf 6) 7 (Leaf 9))

countLeafs :: Tree a -> Int
countLeafs (Leaf _) = 1 -- ! must be wrapped in parentheses
countLeafs (Node l _ r) = countLeafs l + countLeafs r

countNodes :: Tree a -> Int
countNodes (Leaf _) = 0
countNodes (Node l v r) = 1 + countNodes l + countNodes r

-- ignore the tree head, leaf = 1
treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 1
treeDepth (Node l v r) = max (1 + treeDepth l) (1 + treeDepth r)

-- count the tree head, leaf = 0
treeDepth' :: Tree a -> Int
treeDepth' (Leaf _)     = 0
treeDepth' (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)


isSorted :: Ord a => Tree a -> Bool
isSorted (Leaf _)     = True
isSorted (Node l x r) = lessThanEq x l && largerThan x r

lessThanEq :: Ord a => a -> Tree a -> Bool
lessThanEq x (Leaf x')     = x' <= x
lessThanEq x (Node l x' r) = x' <= x && lessThanEq x' l && largerThan x' r

largerThan :: Ord a => a -> Tree a -> Bool
largerThan x (Leaf x')     = x' > x
largerThan x (Node l x' r) = x' > x && lessThanEq x' l && largerThan x' r


-- Integral a => a -> Bool






-- isOdd x = x `mod` y == 0
-- isOdd :: Integral a => a -> a -> Bool

data Ability = High | Low deriving Show

instance Eq Ability where
  (==) High High = True
  (==) Low  Low  = True
  (==) _    _    = False

  -- (/=) = not (==)