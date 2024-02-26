module Class_02_22_24.Codes where
import Class_02_20_24.Introduccion (suma, lista)

-- Función lambda:
{- 
    filter (\x -> mod x 2 == 0) lista
    (\x y -> x + y) 2 4
 -}
-- Lambda Function
add :: Int -> (Int -> Int)
-- Currying
add = \x -> (\y -> x + y)


myFunctions :: [Int -> Int -> Int]
myFunctions = [(+), (-), (*), div, suma]

getFunction :: Char -> (Int -> Int -> Int)
getFunction e
    | e == '+' = head myFunctions
    | e == '-' = myFunctions !! 1
    | e == '*' = myFunctions !! 2
    | e == '/' = myFunctions !! 3
    | otherwise = myFunctions !! 4


-- myExp :: Char -> (Int -> Int -> Int)
-- myExp e = getFunction e

-- Curried Function
multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z

-- multThree 2 3 4
-- ((multThree 2) 3) 4

-- (x³ | x E (1..5))
--  se lee como "x al cubo, donde x pertenece al conjunto de números enteros en el rango de 1 a 5".
-- En haskell: [x ^ 3 | x <- [1..5]]

-- Combinatoria
-- [(x, y) | x <- [1..5], y <- [1..5]]

zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys = [(xs !! i, ys !! i) | i <- [0..min (length xs) (length ys) - 1]]

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort menor ++ [x] ++ qsort mayor
    where 
        menor = [me | me <- xs, me < x]
        mayor = [ma | ma <- xs, ma >= x]

-- concatenación de listas ++
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n 2


esPrimoAux :: Int -> Int -> Bool
esPrimoAux n divisor
    | divisor * divisor > n = True
    | mod n divisor == 0 = False
    | otherwise = esPrimoAux n (divisor + 1)