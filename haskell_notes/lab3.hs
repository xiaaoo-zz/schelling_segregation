-- 1
addition :: Int -> Int -> Int
addition x 0 = x
addition x y = 1 + addition x (y - 1)

-- 2
multiplication :: Int -> Int -> Int
multiplication x 0 = 0
multiplication x y = (addition 0 x) + multiplication x (y - 1)

multiplication' x y = foldr (+) x []

-- 3
powerHelper :: Int -> Int -> [Int]
powerHelper x 0 = []
powerHelper x y = [x] ++ powerHelper x (y - 1)
power :: Int -> Int -> Int
power x y = foldr (*) 1 (powerHelper x y)

-- 3.2 recursive
powerof2 :: Int -> [Int]
-- powerof2 2 -> [0,2,4]
powerof2 0 = [0]
-- ! you cannot write [power (2 x)], no wrapping parentheses
powerof2 x = [power 2 x] ++ (powerof2 (x - 1))

-- 4 division



