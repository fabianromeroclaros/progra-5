module Class_02_26_24.Code where

-- Funciones polimórficas
suma :: (Num a) => a -> a -> a
suma a b = a + b

-- Recursividad
fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength(_:xs) = 1 + myLength xs
-- (_:xs) todos menos el primero


insertVal :: Ord a => a -> [a] -> [a]
insertVal x [] =[x]
insertVal x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : insertVal x ys


myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

