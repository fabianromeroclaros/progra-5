module Examples where

-- Ejemplo 1: Suma
sumaCurried :: Int -> (Int -> Int)
sumaCurried x = \y -> x + y


-- Ejemplo 2: Filtrar array
filtrarCurried :: (a -> Bool) -> ([a] -> [a])
filtrarCurried f = \xs -> [x | x <- xs, f x]

-- Función para usar en filtrarCurried
esPar :: Int -> Bool
esPar x = x `mod` 2 == 0


-- Ejemplo 3: Función para concatenar dos strings con un espacio en medio
concatenarCurried :: String -> (String -> String)
concatenarCurried xs = \ys -> xs ++ " " ++ ys


-- Ejemplo 4: Función para sumar un elemento a todos los elementos de una lista
sumarATodos :: Int -> ([Int] -> [Int])
sumarATodos = \x -> map (\y -> x + y)


-- Ejemplo 5: Retornar el máximo entre dos números
maximoCurried :: Int -> (Int -> Int)
maximoCurried x = \y -> if x > y then x else y

